package main;

import utils.DateConvert;

import java.util.ArrayList;

public class DailySchedule {

    private final int year;
    private final int month;
    private final int day;
    private final int dayOfWeek;
    private final int dayFromNewYear;
    private final ArrayList<Schedule> scheduleList;

    /**
     *
     * @param year 年
     * @param dayFromNewYear 日：1月1日を0日とする経過数（2月2日なら32など）
     *                       この数え方で行くと，各月1日は，1月から順に
     *                       0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365(翌年1月)
     * @param dayOfWeek 曜日：（0:日曜日, 1:月曜日...6:土曜日）
     */
    public DailySchedule(int year, int dayFromNewYear, int dayOfWeek) {
        this.year = year;
        this.dayOfWeek = dayOfWeek;

        int[] date = DateConvert.settingDate(dayFromNewYear);
        this.month = date[0];
        this.day = date[1];
        this.dayFromNewYear = dayFromNewYear;

        scheduleList = new ArrayList<>();
    }



    public void addSchedule(int startTime, int endTime, String content, ScheduleType type){
        Schedule newSchedule = new Schedule(startTime, endTime, content, type);
        scheduleList.add(newSchedule);
    }

    public void fixSchedule(int index, Schedule fixed){
        scheduleList.set(index, fixed);
    }

    public ArrayList<Schedule> getScheduleList() {
        return scheduleList;
    }

    public int getYear() {
        return year;
    }

    public int getMonth() {
        return month;
    }

    public int getDay() {
        return day;
    }

    public int getDayOfWeek() {
        return dayOfWeek;
    }

    public int getDayFromNewYear() {
        return dayFromNewYear;
    }
}
