package es.lavanda.feed.films.util.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import es.lavanda.feed.films.exception.FeedFilmsException;
import es.lavanda.lib.common.model.FilmModelTorrent;
import es.lavanda.lib.common.model.TorrentModel.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

@Slf4j
public class PctfenixFilmsParser extends AbstractFilmsParser {

    private static final String URL_PCTFENIX_BLURAY1080 = "https://pctfenix.com/descargar-peliculas/bluray-1080p/";
    private static final String URL_PCTFENIX_MICRO4K = "https://pctfenix.com/descargar-peliculas/4k-uhdmicro/";
    private static final String URL_PCTFENIX_HTTPS = "https://pctfenix.com";
    private static final String URL_HTTPS = "https:";
    private static final String DATA_UT = "data-ut";
    private static final String REGEX_DATE = "((0[1-9]|[12]\\d|3[01])\\/(0[1-9]|1[0-2])\\/([12]\\d{3}))";
    private static final String REGEX_DATE_ONLY_YEAR = "(\\d{4}|\\d{4}-\\d{4})";

    protected List<FilmModelTorrent> execute() {
        List<FilmModelTorrent> filmsFullhd = getLastFilmsByQuality();
        return filmsFullhd;
    }

    private List<FilmModelTorrent> getLastFilmsByQuality() {
        List<FilmModelTorrent> filmsFullhd = new ArrayList<>();
        return filmsFullhd;
    }

    public void getNewShowsBluray1080() {
        log.info("Ejecutando Pctfenix feed shows Parser");
        String textNewShows = getHTML(URL_PCTFENIX_BLURAY1080);
        Document doc = Jsoup.parse(textNewShows);
        Elements listLi = doc.getElementsByClass("slide-it slick-slide slick-current slick-active");
        for (Element element : listLi) {
            FilmModelTorrent filmModel = new FilmModelTorrent();
            filmModel.setTorrentPage(Page.PCTFENIX);
            getImage(element, filmModel);
            try {
                String urlOfElement = getUrlOfElement(element);
                fillFilm(urlOfElement, filmModel, "bluray 1080p");
                if (!Objects.isNull(filmModel.getTorrentUrl())) {
                    log.info("Sending to download: {}", filmModel.toString());
                    // producerService.sendFilm(filmModel);
                }
            } catch (Exception e) {
                log.error("Show not filled", e);
            }
        }
        log.info("Se ha terminado de ejecutar FeedDownloadShowDescargas2020Impl Parser");
    }

    public void getNewShowsMicro4K() {
        log.info("Ejecutando Pctfenix feed shows Parser");
        String textNewShows = getHTML(URL_PCTFENIX_MICRO4K);
        Document doc = Jsoup.parse(textNewShows);
        Elements listLi = doc.getElementsByClass("slide-it slick-slide slick-current slick-active");
        for (Element element : listLi) {
            FilmModelTorrent filmModel = new FilmModelTorrent();
            filmModel.setTorrentPage(Page.PCTFENIX);
            getImage(element, filmModel);
            try {
                String urlOfElement = getUrlOfElement(element);
                fillFilm(urlOfElement, filmModel, "4k uhdmicro");
                if (!Objects.isNull(filmModel.getTorrentUrl())) {
                    log.info("Sending to download: {}", filmModel.toString());
                    // producerService.sendFilm(filmModel);
                }
            } catch (Exception e) {
                log.error("Show not filled", e);
            }
        }
        log.info("Se ha terminado de ejecutar FeedDownloadShowDescargas2020Impl Parser");
    }

    private void fillFilm(String urlOfElement, FilmModelTorrent filmModel, String quality) {
        String textShow = getHTML(urlOfElement);

        Document doc = Jsoup.parse(textShow);
        Elements elementsName = doc.getElementsByClass("title-hd");
        String title = elementsName.get(0).getElementsByTag("h2").text();
        String torrent = null;
        Elements boxContentDesc = doc.getElementsByClass("box-content-desc");
        Integer year = 0;
        for (Element element : boxContentDesc) {
            year = getYear(element.text());
            if (year != 0) {
                break;
            }
        }
        Elements breadcums = doc.getElementsByClass("breadcrumbs");
        String cleanTitle = "";
        for (Element element : breadcums) {
            cleanTitle = getCleanTitle(element);
            if (Boolean.TRUE.equals(StringUtils.hasText(cleanTitle))) {
                break;
            }
        }

        Elements boxContentLinkx = doc.getElementsByClass("ctn-download-torrent");

        for (Element element : boxContentLinkx) {
            Elements links = element.getElementsByAttribute(DATA_UT);
            torrent = getTorrent(links);

        }
        filmModel.setTorrentYear(year);
        filmModel.setTorrentTitle(title);
        filmModel.setTorrentCroppedTitle(cleanTitle);
        filmModel.setTorrentQuality(quality);
        filmModel.setTorrentUrl(torrent);
    }

    private String getCleanTitle(Element element) {
        Elements strongElements = element.getElementsByTag("strong");
        for (Element element2 : strongElements) {
            if (Boolean.TRUE.equals(StringUtils.hasText(element2.text()))) {
                return element2.text();
            }
        }
        return "";
    }

    private Integer getYear(String text) {
        Matcher matcherDate = Pattern.compile(REGEX_DATE).matcher(text);
        Matcher matcherDateonlyYear = Pattern.compile(REGEX_DATE_ONLY_YEAR).matcher(text);
        if (matcherDate.find()) {
            return Integer.parseInt(matcherDate.group(4));
        } else if (matcherDateonlyYear.find()) {
            return Integer.parseInt(matcherDateonlyYear.group(1));
        }
        return 0;
    }

    private String getTorrent(Elements elements) {
        if (elements.size() == 1 && !elements.get(0).attr(DATA_UT).isEmpty()) {
            return URL_HTTPS + elements.get(0).attr(DATA_UT);
        } else {
            throw new FeedFilmsException("No torrent on this page");
        }
    }

    private String getUrlOfElement(Element element) {
        String url = "";
        Elements elements = element.getElementsByTag("a");
        for (Element element2 : elements) {
            url = URL_PCTFENIX_HTTPS + element2.attr("href");
        }
        return url;
    }

    private void getImage(Element element, FilmModelTorrent filmModel) {
        Elements elements = element.getElementsByClass("mv-img");
        for (Element element2 : elements) {
            if (element2.getElementsByTag("img").size() == 1) {
                String img = URL_HTTPS + element2.getElementsByTag("img").get(0).attr("src");
                filmModel.setTorrentImage(img);
            } else {
                log.debug("not image on this element: {}", element2.text());
            }

        }
    }

    /**
     * Method to get html from a uri
     * 
     * @param urlHtml the uri to realize the get request
     * @return string with the content of the uri
     * @throws Exception
     * @throws AutomatizeTasksException
     */
    private String getHTML(String urlHtml) {
        String s2 = "";
        try {
            // log.debug("Getting html from : {}", urlHtml);
            URL url = new URL(urlHtml);
            HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
            if (urlConnection.getResponseCode() == 200) {
                Reader in2 = new BufferedReader(new InputStreamReader(urlConnection.getInputStream()));
                StringBuilder sb = new StringBuilder();
                for (int c; (c = in2.read()) >= 0;)
                    sb.append((char) c);
                String response = sb.toString();
                s2 = new String(response.getBytes(StandardCharsets.ISO_8859_1), StandardCharsets.UTF_8);
            }
            // log.debug("Response getHTML: " + s2);
            return s2;
        } catch (IOException e) {
            log.error("IO Exception getting html from: {}", urlHtml, e);
            throw new FeedFilmsException(e);
        }
    }

}
