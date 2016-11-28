import argparse
import os

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument()
    
    path = os.path.dirname(__file__)
    print(path)

def find_library_path():
    library_dir = os.path.dirname(__file__)
    fortran_dir = os.path.join(library_dir, "fortran")
    fortran_lib = os.path.join(fortran_dir, "libF2x.so")

    if os.path.exists(fortran_lib):
        return fortran_lib
    else:
        raise FileNotFoundError("Could not find libF2x.so. Maybe you need to build it?")

if __name__ == '__main__':
    main()
