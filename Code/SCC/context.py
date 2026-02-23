import os
import platform
from pathlib import Path


def projectpath():
    return Path(__file__).parents[2]


def local_rice_path():
    return Path("C:/Users/Granella/Dropbox (CMCC)/PhD/Research/RICE50x")


def remote_rice_path():
    return Path("/work/cmcc/fg12520/RICE50x")


def rice_path():
    if platform.system() == 'Windows':
        return local_rice_path()
    else:
        return remote_rice_path()


def pdsettings():
    import pandas as pd
    pd.set_option('display.max_rows', 50)
    pd.set_option('display.max_columns', 500)
    pd.set_option('display.width', 1000)