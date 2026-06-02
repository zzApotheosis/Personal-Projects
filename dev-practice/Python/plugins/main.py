#!/bin/env python3
 
import importlib.util
import pathlib
import sys

class Main(object):
    @staticmethod
    def main():
        exit_code: int = 0

        app = Main()
        app.do_example()

        return exit_code

    def __init__(self):
        self.plugins = None

    def do_example(self):
        self.plugins = self.load_all_plugins('plugins')

        sys.stderr.write("Loaded plugins: {}\n".format(list(self.plugins.keys())))

        for name, module in self.plugins.items():
            if hasattr(module, 'run'):
                sys.stderr.write("Triggering {}:\n".format(name))
                module.run() # This should match Main.plugin_entrypoint
            else:
                sys.stderr.write("Plugin '{}' has no 'run()' function.".format(name))

    def load_plugin(self, file_path: pathlib.Path):
        module_name = file_path.stem
    
        try:
            spec = importlib.util.spec_from_file_location(module_name, file_path)
            if spec is None or spec.loader is None:
                sys.stderr.write("Could not load spec for {}\n".format(file_path))
                return None
    
            module = importlib.util.module_from_spec(spec)
    
            sys.modules[module_name] = module
    
            spec.loader.exec_module(module)
    
            return module
    
        except Exception as e:
            sys.stderr.write("Error loading module {}: {}\n".format(module_name, e))
            return None
    
    def load_all_plugins(self, directory_name: str):
        plugin_dir = pathlib.Path(__file__).parent / directory_name
        loaded_plugins = {}
    
        for file_path in plugin_dir.glob('*.py'):
            module = self.load_plugin(file_path)
            if module:
                loaded_plugins[file_path.stem] = module
    
        return loaded_plugins

if __name__ == '__main__':
    sys.exit(Main.main())


