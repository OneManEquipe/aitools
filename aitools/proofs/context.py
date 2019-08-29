import threading
from functools import wraps
from inspect import isgeneratorfunction
from typing import Iterable

from aitools.logic import Expression
from aitools.logic.utils import LogicObjectSource, VariableSource
from aitools.proofs.proof import Proof


def make_property(attr_name):
    def _make_getter(attr):
        def getter(self):
            return getattr(self._local, attr)
        return getter

    def _make_setter(attr):
        def setter(self, value):
            setattr(self._local, attr, value)

        return setter

    return property(_make_getter(attr_name), _make_setter(attr_name))


class Context():
    def __init__(self):
        self._local = threading.local()

    kb = make_property('kb')
    predicate_source = make_property('predicate_source')
    variable_source = make_property('variable_source')


context = Context()
context.kb = None
context.predicate_source = LogicObjectSource()
context.variable_source = VariableSource()


def prove(formula: Expression, truth: bool = True) -> Iterable[Proof]:
    return context.kb.prove(formula, truth)


def contextual(attribute_name, value):
    def decorator(func):
        is_generator = isgeneratorfunction(func)
        @wraps(func)
        def _wrapper(*args, **kwargs):
            nonlocal is_generator
            previous = getattr(context, attribute_name)
            setattr(context, attribute_name, value)

            res = func(*args, **kwargs)

            if is_generator:
                for r in res:
                    setattr(context, attribute_name, previous)
                    yield r
                    previous = getattr(context, attribute_name)
                    setattr(context, attribute_name, value)

            else:
                setattr(context, attribute_name, previous)
                return res

        return _wrapper

    return decorator