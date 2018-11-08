import os
import subprocess


def _is_test_dir(path):
    return os.path.isdir(path)


def discover_tests():
    test_directory = os.path.dirname(__file__)

    tests = []

    for entry_name in os.listdir(test_directory):
        subdir_path = os.path.join(test_directory, entry_name)

        if not _is_test_dir(subdir_path):
            continue

        source_files = []
        test_files = []

        for filename in os.listdir(subdir_path):
            if any(map(filename.endswith, ['.f90', '.f', '.F'])):
                source_files.append(filename)

            elif filename.endswith('_test.py') or \
                    (filename.startswith('test_') and filename.endswith('.py')):
                test_files.append(filename)

        tests.append((entry_name, subdir_path, source_files, test_files))

    return tests


def build_test(name, testdir, sourcefiles):
    old_dir = os.getcwd()
    os.chdir(testdir)

    print(f"Building tests in {name}...")
    templates = []
    for template in 'bindc/_glue.f90.t', 'ctypes/_glue.py.t', 'cerr/_cerr.c.t':
        templates.append('-t')
        templates.append('@' + template)

    f2x_run = subprocess.Popen(['F2x'] + templates + sourcefiles, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = f2x_run.communicate()

    if f2x_run.returncode:
        print(f2x_run.returncode)

    os.chdir(old_dir)


if __name__ == '__main__':
    tests = discover_tests()

    for name, testdir, sources, testcases in tests:
        build_test(name, testdir, sources)
