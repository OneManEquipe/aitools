import asyncio
from asyncio import QueueEmpty
from threading import Thread
from typing import AsyncIterable


def is_inside_task():
    try:
        asyncio.current_task()
    except RuntimeError:
        return False
    else:
        return True


async def wrap_item(item):
    yield item


async def collect(async_generator: AsyncIterable, queue: asyncio.Queue, poison_pill: object):
    """Collects an asynchronous iterable and pushes each element into a queue, finally appending a poison pill."""
    try:
        async for res in async_generator:
            await queue.put(res)

    finally:
        await queue.put(poison_pill)


async def multiplex(*generators: AsyncIterable, buffer_size: int) -> AsyncIterable:
    """Multiplexes several asynchronous iterables into one"""
    queue = asyncio.Queue(maxsize=buffer_size)

    currently_running_count = len(generators)

    pill = object()
    for generator in generators:
        asyncio.create_task(collect(generator, queue, pill))

    while currently_running_count > 0:
        res = await queue.get()
        if res is pill:
            currently_running_count -= 1
        else:
            yield res


class ThreadSafeishQueue(asyncio.Queue):
    """A partially thread-safe version of an asyncio.Queue.

    It should be safe as long as only one thread writes and only one thread reads.
    """
    # TODO this is the quickest fix I could find but I'm pretty sure it's not actually safe :P
    def __init__(self, *, max_size, loop):
        super().__init__(maxsize=max_size)
        self.loop = loop

    async def __set_result_for_future(self, fut, res):
        await fut.set_result(res)

    def _wakeup_next(self, waiters):
        # TODO yeah I mean, the original was right below a comment stating "End of the overridable methods" :P
        #  what could go wrong?
        # Wake up the next waiter (if any) that isn't cancelled.
        while waiters:
            waiter = waiters.popleft()
            if not waiter.done():
                asyncio.run_coroutine_threadsafe(self.__set_result_for_future(waiter, None), loop=self.loop)
                break


class Scheduler:
    def __init__(self, *, debug=False):
        self.loop = asyncio.new_event_loop()
        self.loop.set_debug(debug)
        self.__thread = Thread(target=self.loop.run_forever, daemon=True)
        self.__thread.start()
        self.__poison_pill = object()

    async def __make_queue(self, max_size):
        return ThreadSafeishQueue(max_size=max_size, loop=self.loop)

    async def __get_from_queue(self, queue: asyncio.Queue):
        return await queue.get()

    def run(self, coroutine):
        return asyncio.run_coroutine_threadsafe(coroutine, self.loop).result()

    def schedule_generator(self, generator: AsyncIterable, *, buffer_size: int):
        queue = asyncio.run_coroutine_threadsafe(self.__make_queue(buffer_size), self.loop).result()

        asyncio.run_coroutine_threadsafe(collect(generator, queue, self.__poison_pill), self.loop)

        while True:
            try:
                el = asyncio.run_coroutine_threadsafe(self.__get_from_queue(queue), self.loop).result()

                while True:
                    if el is self.__poison_pill:
                        return
                    yield el

                    el = queue.get_nowait()
            except QueueEmpty:
                pass