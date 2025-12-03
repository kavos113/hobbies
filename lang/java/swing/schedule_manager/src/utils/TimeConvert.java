package utils;

public class TimeConvert {

    public static String TimeToString(int time){
        String res;

        int[] times = TimeToInt(time);
        res = times[0] + ":" + times[1];

        return res;
    }

    public static int[] TimeToInt(int time){
        int hour = time / 4 + 6;
        int min = time % 4;

        switch (min) {
            case 0 -> {
                min = 30;
                hour--;
            }
            case 1 -> {
                min = 45;
                hour--;
            }
            case 2 -> min = 0;
            case 3 -> min = 15;
        }

        return new int[]{hour, min};
    }

    public static int StringToTime(int hour, int min){
        return (hour - 5) * 4 + min / 15 - 2;
    }

}
