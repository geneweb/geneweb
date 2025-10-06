from pathlib import Path
import shutil
import os

class FileManager:
    """
    File system operations for database management.
    Handles file creation, moving, backup, and directory operations.
    """

    @staticmethod
    def create_dir(path: str, parent: bool = False) -> None:
        """
        Create directory.

        Args:
            path: Directory path
            parent: Create parent directories if True
        """
        Path(path).mkdir(parents=parent, exist_ok=True)

    @staticmethod
    def create_file(path: str) -> None:
        """
        Create empty file.

        Args:
            path: File path
        """
        Path(path).touch()

    @staticmethod
    def remove(path: str) -> None:
        """
        Remove file if exists.

        Args:
            path: File path
        """
        try:
            os.remove(path)
        except FileNotFoundError:
            pass

    @staticmethod
    def remove_rf(path: str) -> None:
        """
        Remove directory recursively.

        Args:
            path: Directory path
        """
        try:
            shutil.rmtree(path)
        except FileNotFoundError:
            pass

    @staticmethod
    def move_with_backup(src: str, dst: str) -> None:
        """
        Move file with backup.

        Creates backup of destination as dst~, then moves src to dst.

        Args:
            src: Source file path
            dst: Destination file path
        """
        backup = dst + "~"
        FileManager.remove(backup)

        if os.path.exists(dst):
            shutil.move(dst, backup)

        shutil.move(src, dst)

    @staticmethod
    def copy_file(src: str, dst: str) -> None:
        """
        Copy file.

        Args:
            src: Source file path
            dst: Destination file path
        """
        shutil.copy2(src, dst)

    @staticmethod
    def safe_rename(src: str, dst: str) -> None:
        """
        Rename with fallback for Windows.

        Args:
            src: Source file path
            dst: Destination file path
        """
        try:
            FileManager.remove(dst)
            os.rename(src, dst)
        except PermissionError:
            # Windows fallback
            FileManager.copy_file(src, dst)
            FileManager.remove(src)
