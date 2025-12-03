package model;

import java.io.File;
import java.util.ArrayList;

public class FileStock {
    private final ArrayList<File> files;

    public FileStock(){
        files = new ArrayList<>();
    }

    /**
     * ファイルをストックに追加する
     * @param file 追加するファイル
     */
    public void addFile(File file){
        files.add(file);
    }

    /**
     * ファイルをストックから削除する
     * @param file 削除するファイル
     */
    public void removeFile(File file){
        files.remove(file);
    }

    /**
     * ファイル名を指定してファイルを削除する(探索)
     * @param fileName 削除するファイル名
     */
    public void removeFile(String fileName){
        for (File file : files){
            if (file.getName().equals(fileName)){
                files.remove(file);
                break;
            }
        }
    }

    /**
     * ストック内のすべてのファイルを削除する
     */
    public void clear() {
        files.clear();
    }

    /**
     * ストック内のファイルを取得する
     * @return ファイルのリスト
     */
    public File getFile(String fileName){
        for (File file : files){
            if (file.getName().equals(fileName)){
                return file;
            }
        }
        return null;
    }

    /**
     * ストック内のすべてのファイルを取得する
     * @return ファイルのリスト
     */
    public ArrayList<File> getFiles() {
        return files;
    }
}
