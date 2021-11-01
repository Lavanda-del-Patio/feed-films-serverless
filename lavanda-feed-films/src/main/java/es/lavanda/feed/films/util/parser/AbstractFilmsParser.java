package es.lavanda.feed.films.util.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.List;

import es.lavanda.feed.films.exception.FeedFilmsException;
import es.lavanda.lib.common.model.FilmModelTorrent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RequiredArgsConstructor
public abstract class AbstractFilmsParser {

    public List<FilmModelTorrent> executeBeans() {
        return execute();
    }

    protected abstract List<FilmModelTorrent> execute();

    /**
     * Method to get html from a uri
     * 
     * @param urlHtml the uri to realize the get request
     * @return string with the content of the uri
     * @throws Exception
     * @throws AutomatizeTasksException
     */
    protected String getHTML(String urlHtml, Charset charset) {
        try {
            log.info("Getting html from : {}", urlHtml);
            URL url = new URL(urlHtml);
            HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
            if (urlConnection.getResponseCode() >= 200 && urlConnection.getResponseCode() <= 399) {
                Reader in2 = new BufferedReader(new InputStreamReader(urlConnection.getInputStream(), charset));
                StringBuilder stringBuilder = new StringBuilder();
                for (int c; (c = in2.read()) >= 0;)
                    stringBuilder.append((char) c);
                return stringBuilder.toString();
            } else {
                throw new FeedFilmsException("HTTP code not 200, is "+urlConnection.getResponseCode());
            }
        } catch (IOException e) {
            log.error("IO Exception getting html from: {}", urlHtml, e);
            throw new FeedFilmsException(e);
        }
    }

}
