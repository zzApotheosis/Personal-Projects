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
        self.other_plugins = list()

    def do_example(self):
        self.plugins = self.load_all_plugins('plugins')
        self.load_other_plugins('other')

        sys.stderr.write("Loaded plugins: {}\n".format(list(self.plugins.keys())))
        sys.stderr.write("Loaded other plugins: {}\n".format([p['path'].stem for p in self.other_plugins]))

        for name, module in self.plugins.items():
            if hasattr(module, 'run'):
                sys.stderr.write("Triggering {}:\n".format(name))
                module.run() # This should match Main.plugin_entrypoint
            else:
                sys.stderr.write("Plugin '{}' has no 'run()' function.".format(name))

        for plugin in self.other_plugins:
            if 'run' in plugin['locals']:
                sys.stderr.write("Triggering {}:\n".format(plugin['path'].stem))
                plugin['locals']['run']()

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

    def load_other_plugins(self, dir_name: str):
        other_dir = pathlib.Path(__file__).parent / dir_name
        for file_path in other_dir.glob('*.py'):
            module_info: dict = dict()
            module_info['path'] = pathlib.Path(file_path)
            module_info['globals'] = dict()
            module_info['locals'] = dict()
            exec(module_info['path'].read_text(encoding='utf-8'), None, module_info['locals'])
            self.other_plugins.append(module_info)
        
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


