import javax.swing.JOptionPane;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class StatesList {

    static String[] answers = {"アラバマ", "アラスカ", "アリゾナ", "アーカンソー", "カリフォルニア", "コロラド",
                        "コネティカット", "デラウェア", "フロリダ", "ジョージア", "ハワイ", "アイダホ",
                        "イリノイ", "インディアナ", "アイオワ", "カンザス", "ケンタッキー", "ルイジアナ",
                        "メーン", "メリーランド", "マサチューセッツ", "ミシガン", "ミネソタ", "ミシシッピ",
                        "ミズーリ", "モンタナ", "ネブラスカ", "ネバダ", "ニューハンプシャー", "ニュージャージー",
                        "ニューメキシコ", "ニューヨーク", "ノースカロライナ", "ノースダコタ", "オハイオ",
                        "オクラホマ", "オレゴン", "ペンシルベニア", "ロードアイランド", "サウスカロライナ",
                        "サウスダコタ", "テネシー", "テキサス", "ユタ", "バーモント", "バージニア", "ワシントン",
                        "ウェストバージニア", "ウィスコンシン", "ワイオミング"};
    static Path[] paths;

    static {
        try(Stream<Path> stream = Files.list(Paths.get("imgs\\states"))) {
            paths = stream.toArray(Path[]::new);

            if(paths.length != answers.length){
                JOptionPane.showMessageDialog(null, "Error", "Error", JOptionPane.ERROR_MESSAGE);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static States getRandom(){
        int random = (int) (Math.random() * 50);

        return new States(answers[random], paths[random]);
    }
}
