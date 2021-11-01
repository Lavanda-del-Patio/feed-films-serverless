package es.lavanda.feed.films;

import es.lavanda.feed.films.util.parser.DonTorrentFilmsParser;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class DonTorrentFilmsParserTest {

    @InjectMocks
    private DonTorrentFilmsParser donTorrentFilmsParser;

    @Test
    public void test() {
        Assertions.assertNotNull(donTorrentFilmsParser);
        Assertions.assertNotNull(donTorrentFilmsParser.getNewFilmsBluray1080());
        Assertions.assertNotNull(donTorrentFilmsParser.getNewShowsMicro4K());
    }
}