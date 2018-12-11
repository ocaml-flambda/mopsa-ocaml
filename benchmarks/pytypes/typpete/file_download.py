"""Mocking built in function urlretrieve and built-in library os

Remove when their stubs are implemented
"""

import mopsa

def urlretrieve(download_path: str, local_path: str) -> None:
    pass


class Path:
    def dirname(self, path: str) -> str:
        return path


class OS:
    path = Path()

    def makedirs(self, dirname: str, exist_ok: bool):
        pass


os = OS()


class File:
    def __init__(self, local_path, download_path, file_name):
        self.local_path = local_path
        self.download_path = download_path
        self.file_name = file_name

    def download(self):
        self.create_folder_if_not_exists()
        urlretrieve(self.download_path, self.local_path)
        print("Downloaded: " + self.local_path)
        return True

    def create_folder_if_not_exists(self):
        """Create new folder(s) in the local path"""
        os.makedirs(os.path.dirname(self.local_path), True)



def test_main():
    f1 = File("home/user", "http://www.download.com/", "file.py")
    ch1 = f1.download()

    f2 = File(f1.local_path, f1.download_path + "2", "file2.py")
    ch2 = f2.download()

    if ch1 and ch2:
        print("DONE!")

    mopsa.assert_safe()
    mopsa.massert(isinstance(f1, File))
    mopsa.massert(isinstance(f1.local_path, str))
    mopsa.massert(isinstance(f1.download_path, str))
    mopsa.massert(isinstance(f1.file_name, str))
    mopsa.massert(isinstance(f2, File))
    mopsa.massert(isinstance(f2.local_path, str))
    mopsa.massert(isinstance(f2.download_path, str))
    mopsa.massert(isinstance(f2.file_name, str))
    mopsa.massert(isinstance(ch1, bool))
    mopsa.massert(isinstance(ch2, bool))
# File := Type[File]
# f1 := File
# f2 := File
# ch1 := bool
# ch2 := bool
