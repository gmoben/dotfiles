import os


SYMLINKS = ".i3 .Xresources .Xresources.d .aliases .config/systemd/user/emacs.service .config/systemd/user/ssh-agent.service .config/systemd/user/xscreensaver.service .emacs.conf .extend.Xresources .extend.profile .pam_environment .pylintrc .tmux .tmux.conf .xinitrc .xmodmap .Xresources .Xresources.d .xprofile .xsession .zshrc".split(' ')


def make_symlinks(origin, target, subpaths):
    for path in subpaths:
        origin_path = os.path.join(origin, path)
        target_path = os.path.join(target, path)
