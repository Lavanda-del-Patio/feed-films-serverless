package es.lavanda.feed.films.exception;

public class FeedFilmsException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public FeedFilmsException(String message, Exception e) {
        super(message, e);
    }

    public FeedFilmsException(String message) {
        super(message);
    }

    public FeedFilmsException(Exception e) {
        super(e);
    }

}
