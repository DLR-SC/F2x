import os

def find_library_path(lang, item=None):
    library_dir = os.path.dirname(__file__)
    lang_dir = os.path.join(library_dir, lang)
    
    if item is not None:
        lang_lib = os.path.join(lang_dir, item)
        if os.path.exists(lang_lib):
            return lang_lib
    
    elif os.path.isdir(lang_dir):
        return lang_dir
    
    raise FileNotFoundError("Could not find {0}/{1}. Maybe you need to build it?".format(lang, item))
