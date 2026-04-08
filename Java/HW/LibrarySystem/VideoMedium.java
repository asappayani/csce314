public class VideoMedium extends LibraryItem {

    private final int totalMinutes;
    private final String rating;

    public VideoMedium(String title, int totalMinutes, String rating) {
        super(title);
        this.totalMinutes = totalMinutes;

        if (rating.strip().equals("G") || rating.strip().equals("PG")){
            this.rating = rating;
        } else {
            this.rating = "PG";
        }
    }

    public int getTotalMinutes() {
        return totalMinutes;
    }

    public String getRating() {
        return rating;
    }

    @Override
    public int getLoanPeriodDays(){
        return 7;
    }
    
}