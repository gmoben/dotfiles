#!/usr/bin/env python3

import errno
import logging
import os
import shutil
import sys

from os.path import (
    abspath, dirname, exists, expanduser,
    join, isdir, isfile, islink
)


LOG = logging.getLogger()
LOG.setLevel(logging.DEBUG)

always_delete = False


def _copy(src, dest):
    try:
        shutil.copytree(src, dest)
    except OSError as e:
        # If the error was caused because the source wasn't a directory
        if e.errno == errno.ENOTDIR:
            shutil.copy(src, dest)
        else:
            print('Directory not copied. Error: %s' % e)


def ask_delete(dest):
    global always_delete

    if islink(dest):
        ftype = 'Symlink'
    elif isfile(dest):
        ftype = 'File'
    elif isdir(dest):
        ftype = 'Directory'

    if not always_delete:
        answer = input(f"{ftype} already exists at {dest}. Delete it? (yes/no/always) ")
        if answer.startswith(('y', 'Y', 'a', 'A')):
            if answer.startswith(('a', 'A')):
                always_delete = True
            if ftype == 'Directory':
                shutil.rmtree(dest)
            else:
                os.remove(dest)
            return True


def _operate(func, src, dest):
    fname = func.__name__.strip('_')

    if not exists(src):
        LOG.error(f"Source file missing at {src}")
        return

    if not exists(dirname(dest)):
        LOG.info(f"Creating destination directory {dirname(dest)}")
        os.makedirs(dirname(dest))

    if exists(dest):
        if not ask_delete(dest):
            LOG.info(f"Skipping {fname} operation from {src} to {dest}")
        return
    else:
        LOG.info(f"{fname.upper()}ing {src} to {dest}")
        func(src, dest)
        return True


def build_paths(src_dir, dest_dir, subpaths):
    subpaths = [path.strip() for path in subpaths]
    return [(join(src_dir, path), join(dest_dir, path)) for path in subpaths]


def create_symlinks(src_dir, dest_dir, subpaths):
    paths = build_paths(src_dir, dest_dir, subpaths)
    LOG.info(f"Creating {len(paths)} symlinks...")
    results = list(map(lambda args: _operate(os.symlink, *args), paths))
    LOG.info(f"{results.count(True)}/{len(paths)} successfully symlinked")


def create_copies(src_dir, dest_dir, subpaths):
    paths = build_paths(src_dir, dest_dir, subpaths)
    LOG.info(f"Copying {len(paths)} files/directories...")
    results = list(map(lambda args: _operate(_copy, *args), paths))
    LOG.info(f"{results.count(True)}/{len(paths)} successfully copied")


def main():
    src_dir = abspath(dirname(abspath(__file__)) + '/..')
    dest_dir = expanduser('~')

    skip_symlinks = any('-skip-symlinks' in x for x in sys.argv[1:])
    skip_copy = any('-skip-copy' in x for x in sys.argv[1:])

    with open("to_symlink") as f:
        to_symlink = f.readlines()

    if not skip_symlinks:
        create_symlinks(src_dir, dest_dir, to_symlink)

    with open("to_copy") as f:
        to_copy = f.readlines()

    if not skip_copy:
        create_copies(src_dir, dest_dir, to_copy)


if __name__ == '__main__':
    main()
