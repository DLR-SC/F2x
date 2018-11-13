from F2x.distutils.strategy.library import ExtensionLibBuildStrategy
from F2x.distutils.strategy.base import BuildStrategy

builtin_strategies = {
    'lib': ExtensionLibBuildStrategy(['bindc', 'ctypes']),
    'lib_err': ExtensionLibBuildStrategy(['bindc', 'cerr', 'ctypes_err']),
}


def register_strategy(name, strategy):
    builtin_strategies[name] = strategy


def get_target_strategy(target_name):
    if target_name in builtin_strategies:
        return builtin_strategies[target_name]

    return BuildStrategy([])