package main;

public enum ScheduleType {
    受講, その他東進or学習, 学校, 生活, 趣味その他, 睡眠;

    public static ScheduleType convertToType(String str){

        return switch (str) {
            case "受講" -> 受講;
            case "その他東進or学習" -> その他東進or学習;
            case "学校" -> 学校;
            case "生活" -> 生活;
            case "趣味その他" -> 趣味その他;
            case "睡眠" -> 睡眠;
            default -> null;
        };

    }
}
