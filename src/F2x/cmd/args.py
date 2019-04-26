# -*- coding: utf8 -*-
# ... DLR license header ...
"""
Adaptation of the Python ArugmentParser that allows to use a configuration file and environment variables.

* If environment variables are set, they are used as `default` values.
* You can add an argument of type `ParamConfigAction` which
"""
import configparser
import os
from argparse import ArgumentParser, Action, _AppendAction


class F2xArgumentParser(ArgumentParser):
    def __init__(self, *args, env_prefix=None, **kwargs):
        self.env_prefix = env_prefix
        self._config_sections = {}
        super().__init__(*args, **kwargs)

    def add_argument(self, *name_or_flags, config_section=None, **kwargs):
        action = super().add_argument(*name_or_flags, **kwargs)

        # If we have an env_prefix, try to get (alternate) default value from environment.
        if self.env_prefix:
            self._get_env_default(action)

        # Keep track of arguments that may be read from config.
        if config_section:
            self._add_config_action(config_section, action)

        return action

    def _get_env_default(self, action):
        env_name = self.env_prefix + action.dest.upper()
        env_value = os.environ.get(env_name)
        if env_value:
            cast = action.type or str
            action.default = cast(env_value)

    def _add_config_action(self, config_section, action):
        if config_section not in self._config_sections:
            self._config_sections[config_section] = []
        self._config_sections[config_section].append(action)

    def add_argument_group(self, *args, config_section=None, **kwargs):
        group = super().add_argument_group(*args, **kwargs)

        # Monkey-patching group to keep track of arguments.
        if config_section:
            group_add_argument = group.add_argument

            def patched_add_argument(*args, **kwargs):
                section = kwargs.pop('config_section', config_section)
                action = group_add_argument(*args, **kwargs)
                if self.env_prefix:
                    self._get_env_default(action)
                self._add_config_action(section, action)

            group.add_argument = patched_add_argument

        return group


class ParamConfigAction(Action):
    def _cast_config_value(self, action, config_value):
        if action.nargs == 0:
            return action.const

        cast = action.type or str
        if isinstance(action, _AppendAction):
            config_value = config_value.split(';')
        else:
            config_value = [config_value]

        if action.nargs is not None:
            config_value = [list(map(cast, value.split(','))) for value in config_value]
        else:
            config_value = list(map(cast, config_value))

        return config_value

    def __call__(self, parser, namespace, values, option_string=None):
        config_file = configparser.ConfigParser(allow_no_value=True)
        config_file.read(values)
        setattr(namespace, self.dest, config_file)

        try:
            for section, actions in parser._config_sections.items():
                if not section in config_file:
                    continue

                for action in actions:
                    if action.dest not in config_file[section]:
                        continue

                    values = self._cast_config_value(action, config_file[section][action.dest])
                    for value in values:
                        action(parser, namespace, value)

        except AttributeError:
            pass
