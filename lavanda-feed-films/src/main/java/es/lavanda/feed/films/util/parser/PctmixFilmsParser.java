package es.lavanda.feed.films.util.parser;

import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
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

@Slf4j
public class PctmixFilmsParser extends AbstractFilmsParser {

    private static final String URL_FILMS_HD = System.getenv("PCTMIX_PAGE") + "/peliculas-hd/";
    private static final String URL_HTTPS = "https:";
    private static final String URL_HTTPS_PTCTMIX = System.getenv("PCTMIX_PAGE");

    private static final Pattern PATTERN_DATE_SPANISH = Pattern
            .compile("((0[1-9]|[12]\\d|3[01])/(0[1-9]|1[0-2])/[12]\\d{3})");
    private static final Pattern PATTERN_YEAR_SPANISH = Pattern.compile("([12]\\d{3})");

    protected List<FilmModelTorrent> execute() {
        return getNewFilms();
    }

    public List<FilmModelTorrent> getNewFilms() {
        List<FilmModelTorrent> newFilms = new ArrayList<>();
        log.info("Ejecutando Pctmix feed films Parser");
        String textNewFilms = getHTML(URL_FILMS_HD, StandardCharsets.ISO_8859_1);
        Document doc = Jsoup.parse(textNewFilms);
        Elements listLi = doc.getElementsByClass("pelilist");
        for (Element element : listLi) {
            Elements elementFilms = element.getElementsByTag("li");
            for (Element elementFilm : elementFilms) {
                String quality = getQualityOfElement(elementFilm);
                for (Element elementUrl : elementFilm.getElementsByTag("a")) {
                    if (Boolean.TRUE.equals(elementUrl.hasAttr("href"))) {
                        try {
                            FilmModelTorrent filmModelTorrent = new FilmModelTorrent();
                            filmModelTorrent.setTorrentPage(Page.PCTMIX);
                            analizeFilm(elementUrl.attr("href"), filmModelTorrent, quality);
                            newFilms.add(filmModelTorrent);
                        } catch (FeedFilmsException e) {
                            log.info("Error parsing film with url {}", elementUrl.attr("href"), e);
                        }
                    }
                }
            }
        }
        log.info("Finalizado ejecución Pctmix feed films Parser");
        return newFilms;
    }

    private String getQualityOfElement(Element elementFilm) {
        if (elementFilm.hasText()) {
            for (Element spanElements : elementFilm.getElementsByTag("span")) {
                log.info(spanElements.text());
                return spanElements.text();
            }
        }
        return "";
    }

    private void analizeFilm(String urlFilm, FilmModelTorrent filmModelTorrent, String quality) {
        log.info("Analize film by url {}", urlFilm);
        String textNewFilm = getHTML(urlFilm, StandardCharsets.ISO_8859_1);
        Document doc = Jsoup.parse(textNewFilm, "UTF_8");
        Elements filmElements = doc.getElementsByAttributeValue("id", "content-ficha");
        Element filmElement = filmElements.first();
        Elements images = filmElement.getElementsByTag("img");
        if (Boolean.FALSE.equals(images.isEmpty() && images.size() == 2)) {
            filmModelTorrent.setTorrentImage(URL_HTTPS + images.get(1).attr("src"));
        }
        Elements titleElements = filmElement.getElementsByClass("page-box");
        if (Boolean.FALSE.equals(titleElements.isEmpty())) {
            Elements titleH1Elements = titleElements.get(0).getElementsByTag("h1");
            if (Boolean.FALSE.equals(titleH1Elements.isEmpty())) {
                filmModelTorrent.setTorrentTitle(titleH1Elements.first().text());
            }
        }
        Elements sinopsisElements = filmElement.getElementsByClass("sinopsis");
        Elements descriptionTopElements = filmElement.getElementsByClass("descripcion_top");

        if (Boolean.FALSE.equals(sinopsisElements.isEmpty())
                && Boolean.FALSE.equals(descriptionTopElements.isEmpty())) {
            Matcher matcherDate = PATTERN_DATE_SPANISH.matcher(sinopsisElements.first().text());
            Matcher matcherYear = PATTERN_YEAR_SPANISH.matcher(descriptionTopElements.first().text());
            if (matcherDate.find()) {
                DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
                LocalDate localDate = LocalDate.parse(matcherDate.group(0), dateTimeFormatter);
                filmModelTorrent.setTorrentYear(localDate.getYear());
            }
            if (matcherYear.find()) {
                filmModelTorrent.setTorrentYear(Integer.parseInt(matcherYear.group(0)));
            }
        }
        filmModelTorrent.setTorrentQuality(quality);
        Elements sizeElements = doc.getElementsContainingOwnText("Size:");
        if (Boolean.FALSE.equals(sizeElements.isEmpty())) {
            filmModelTorrent.setTorrentSize(sizeElements.first().parent().text().split("Size: ")[1]);
        }
        filmModelTorrent.setTorrentUrl(getTorrentUrl(filmElement));
        Elements torrentName = doc.getElementsByTag("a");
        if (Boolean.FALSE.equals(torrentName.isEmpty())) {
            for (Element element : torrentName) {
                for (Element torrentNameTag : element.getElementsByAttributeValue("href", urlFilm)) {
                    if (torrentNameTag.attributes().size() == 1) {
                        filmModelTorrent.setTorrentCroppedTitle(torrentNameTag.text());
                        break;
                    }
                }
            }
        }
        log.info(filmModelTorrent.toString());
    }

    private String getTorrentUrl(Element filmElement) {
        try {
            String secondPage = URL_HTTPS_PTCTMIX
                    + filmElement.html().split("window.location.href = \"")[1].split("\";")[0];
            String thirdPage = URL_HTTPS
                    + this.getHTML(secondPage, StandardCharsets.ISO_8859_1).split("window.location.href = \"")[1]
                            .split("\";")[0];
            return this.getHTML(thirdPage, StandardCharsets.ISO_8859_1).split("data-u=\"")[1].split("\"")[0];
        } catch (ArrayIndexOutOfBoundsException e) {
            log.error("Not found window location href", (Throwable) e);
            throw new FeedFilmsException("Not found torrent URL");
        }
    }
}
