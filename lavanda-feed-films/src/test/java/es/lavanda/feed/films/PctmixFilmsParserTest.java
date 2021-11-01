package es.lavanda.feed.films;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import es.lavanda.feed.films.util.parser.PctmixFilmsParser;

@ExtendWith(MockitoExtension.class)
public class PctmixFilmsParserTest {
    @InjectMocks
    private PctmixFilmsParser pctmixFilmsParser;

    @Test
    public void test() {
        Assertions.assertNotNull(pctmixFilmsParser);
        Assertions.assertDoesNotThrow(() -> pctmixFilmsParser.getNewFilms());
    }
}
