#!env python3

import fire


BRIGHTNESS_PATH = '/sys/class/backlight/intel_backlight/brightness'


def read(path, cast=None):
    content = ''
    with open(path, 'r') as f:
        content = f.read()
    return cast(content) if cast is not None else content


def write(path, value, binary=False):
    perms = 'wb' if binary else 'w'
    with open(path, perms) as f:
        f.write(value)


class Brightness(object):

    @property
    def current(self):
        return self.read()

    def read(self):
        return read(BRIGHTNESS_PATH, int)

    def write(self, value):
        value = str(value)
        return write(BRIGHTNESS_PATH, value)

    def inc(self, amount):
        new = self.current + int(amount)
        self.write(new)

    def dec(self, amount):
        new = max((self.current - int(amount)), 0)
        self.write(new)


if __name__ == '__main__':
    fire.Fire(Brightness)
