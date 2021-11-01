package es.lavanda.feed.films;

import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestStreamHandler;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import es.lavanda.feed.films.util.parser.DonTorrentFilmsParser;
import es.lavanda.lib.common.model.FilmModelTorrent;

@Slf4j
public class DonTorrentApp implements RequestStreamHandler {

    private static final ObjectMapper MAPPER = createObjectMapper();

    private static ObjectMapper createObjectMapper() {
        return new ObjectMapper().findAndRegisterModules().enable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
    }

    public void handleRequest(InputStream input, OutputStream output, Context context) throws IOException {
        log.info("DonTorrent Parser");
        DonTorrentFilmsParser donTorrentFilmsParserBean = new DonTorrentFilmsParser();
        List<FilmModelTorrent> response = donTorrentFilmsParserBean.executeBeans();
        MAPPER.writeValue(output, response);
    }
}
