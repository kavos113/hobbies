package utils;

public class DateConvert {

    public static int[] settingDate(int dayFromNY){
        int monthTemp = 0, dayTemp = 0;
        if(dayFromNY >= 334){
            monthTemp = 12;
            dayTemp = dayFromNY - 333;
        }else if (dayFromNY >= 304){
            monthTemp = 11;
            dayTemp = dayFromNY - 303;
        }else if (dayFromNY >= 273){
            monthTemp = 10;
            dayTemp = dayFromNY - 272;
        }else if (dayFromNY >= 243){
            monthTemp = 9;
            dayTemp = dayFromNY - 242;
        }else if (dayFromNY >= 212){
            monthTemp = 8;
            dayTemp = dayFromNY - 211;
        }else if (dayFromNY >= 181){
            monthTemp = 7;
            dayTemp = dayFromNY - 180;
        }else if (dayFromNY >= 151){
            monthTemp = 6;
            dayTemp = dayFromNY - 150;
        }else if (dayFromNY >= 120){
            monthTemp = 5;
            dayTemp = dayFromNY - 119;
        }else if (dayFromNY >= 90){
            monthTemp = 4;
            dayTemp = dayFromNY - 89;
        }else if (dayFromNY >= 59){
            monthTemp = 3;
            dayTemp = dayFromNY - 58;
        }else if (dayFromNY >= 31){
            monthTemp = 2;
            dayTemp = dayFromNY - 30;
        }else if (dayFromNY >= 0){
            monthTemp = 1;
            dayTemp = dayFromNY + 1;
        }

        return new int[]{monthTemp, dayTemp};
    }

}
