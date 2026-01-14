#!/usr/bin/env python3

import argparse
import errno
import os
import shutil
import sys

from os.path import (
    abspath, dirname, exists, expanduser,
    join, isdir, isfile, islink
)

import structlog


LOG = structlog.getLogger()

always_delete = False

# Distro configuration mapping distro names to their package lists
# Only includes system package manager lists (yay, apt, etc.)
DISTRO_CONFIG = {
    'arch': {
        'pkglists': ['arch/yay.pkglist'],
    },
    'ubuntu': {
        'pkglists': ['ubuntu/apt.pkglist'],
    },
    'rpi': {
        'pkglists': ['rpi/apt.pkglist'],
    },
    'darwin': {
        'pkglists': [],  # brew - no pkglist currently
    },
    'al2': {
        'pkglists': [],  # yum - no pkglist currently
    },
}


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

    ftype = 'Unknown'
    if islink(dest):
        ftype = 'Symlink'
    elif isfile(dest):
        ftype = 'File'
    elif isdir(dest):
        ftype = 'Directory'

    should_delete = always_delete
    if not always_delete:
        answer = input(f"{ftype} already exists at {dest}. Delete it? (yes/no/always) ")
        if answer.startswith(('y', 'Y', 'a', 'A')):
            if answer.startswith(('a', 'A')):
                always_delete = True
            should_delete = True

    if should_delete:
        if ftype == 'Directory':
            shutil.rmtree(dest)
        else:
            os.remove(dest)
        return True
    return False


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

    LOG.info(f"{fname.capitalize()}ing {src} to {dest}")
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


def load_packages_for_distro(setup_dir, distro):
    """Load all packages from the distro's package lists."""
    packages = set()
    config = DISTRO_CONFIG.get(distro, {})
    pkglists = config.get('pkglists', [])

    for pkglist_path in pkglists:
        full_path = join(setup_dir, pkglist_path)
        if exists(full_path):
            with open(full_path) as f:
                for line in f:
                    line = line.strip()
                    # Skip empty lines and comments
                    if line and not line.startswith('#'):
                        packages.add(line)
        else:
            LOG.warning(f"Package list not found: {full_path}")

    return packages


def parse_conditional_entry(entry):
    """
    Parse an entry that may have conditional syntax.

    Supports:
        path:if-pkg:PACKAGE - only include if PACKAGE is in pkglist

    Returns:
        (path, condition_type, condition_value) or (path, None, None)
    """
    entry = entry.strip()

    if ':if-pkg:' in entry:
        parts = entry.split(':if-pkg:')
        if len(parts) == 2:
            return (parts[0], 'if-pkg', parts[1])

    return (entry, None, None)


def filter_entries(entries, distro, setup_dir):
    """
    Filter entries based on conditional syntax.

    Returns list of paths that should be processed.
    """
    packages = load_packages_for_distro(setup_dir, distro)
    filtered = []

    for entry in entries:
        entry = entry.strip()
        if not entry or entry.startswith('#'):
            continue

        path, condition_type, condition_value = parse_conditional_entry(entry)

        if condition_type is None:
            # No condition, always include
            filtered.append(path)
        elif condition_type == 'if-pkg':
            if condition_value in packages:
                LOG.info(f"Including {path} (condition: {condition_value} in pkglist)")
                filtered.append(path)
            else:
                LOG.info(f"Skipping {path} (condition: {condition_value} not in pkglist)")

    return filtered


def parse_args():
    parser = argparse.ArgumentParser(
        description='Install dotfiles by creating symlinks and copying files.'
    )
    parser.add_argument(
        '--distro',
        required=True,
        choices=list(DISTRO_CONFIG.keys()),
        help=f'Target distribution. Valid options: {", ".join(DISTRO_CONFIG.keys())}'
    )
    parser.add_argument(
        '-skip-symlinks',
        action='store_true',
        help='Skip creating symlinks'
    )
    parser.add_argument(
        '-skip-copy',
        action='store_true',
        help='Skip copying files'
    )
    return parser.parse_args()


def main():
    args = parse_args()

    src_dir = abspath(dirname(abspath(__file__)) + '/..')
    dest_dir = expanduser('~')
    setup_dir = join(src_dir, 'setup')

    with open(f"{setup_dir}/to_symlink") as f:
        to_symlink_raw = f.readlines()

    to_symlink = filter_entries(to_symlink_raw, args.distro, setup_dir)

    if not args.skip_symlinks:
        create_symlinks(src_dir, dest_dir, to_symlink)

    with open(f"{setup_dir}/to_copy") as f:
        to_copy_raw = f.readlines()

    to_copy = filter_entries(to_copy_raw, args.distro, setup_dir)

    if not args.skip_copy:
        create_copies(src_dir, dest_dir, to_copy)


if __name__ == '__main__':
    main()
