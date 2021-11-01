package es.lavanda.feed.films.util.parser;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.amazonaws.util.NumberUtils;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import es.lavanda.lib.common.model.FilmModelTorrent;
import es.lavanda.lib.common.model.TorrentModel.Page;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class DonTorrentFilmsParser extends AbstractFilmsParser {

    private static final String URL_DONTORRENTS_BLURAY1080 = "https://dontorrent.vip/descargar-peliculas/hd";
    private static final String URL_DONTORRENTS_4K = "https://dontorrent.vip/peliculas/4K";
    private static final String URL_DONTORRENTS_DOMAIN = "https://dontorrent.vip";
    private static final String URL_HTTPS = "https:";
    private static final String BLURAY_STRING = "BluRay 1080p";
    private static final String MICROHD_4K = "4K UHDmicro";

    protected List<FilmModelTorrent> execute() {
        List<FilmModelTorrent> filmModelTorrents = new ArrayList<>();
        filmModelTorrents.addAll(getNewFilmsBluray1080());
        filmModelTorrents.addAll(getNewShowsMicro4K());
        return filmModelTorrents;
    }

    public List<FilmModelTorrent> getNewFilmsBluray1080() {
        List<FilmModelTorrent> newFilms = new ArrayList<>();
        log.info("Ejecutando DonTorrent feed films Parser on 1080p");
        String textNewFilms = getHTML(URL_DONTORRENTS_BLURAY1080, StandardCharsets.UTF_8);
        Document doc = Jsoup.parse(textNewFilms);
        Elements listLi = doc.getElementsByClass("noticiasContent");
        for (Element element : listLi) {
            Elements elementFilms = element.getElementsByClass("text-center");
            for (Element elementFilm : elementFilms) {
                for (Element elementUrl : elementFilm.getElementsByTag("a")) {
                    if (Boolean.TRUE.equals(elementUrl.hasAttr("href"))) {
                        FilmModelTorrent filmModelTorrent = new FilmModelTorrent();
                        filmModelTorrent.setTorrentPage(Page.DON_TORRENT);
                        filmModelTorrent.setTorrentQuality(BLURAY_STRING);
                        analizeFilm(URL_DONTORRENTS_DOMAIN + elementUrl.attr("href"), filmModelTorrent);
                        newFilms.add(filmModelTorrent);
                    }
                }
            }
        }
        log.info("Finalizado ejecución DonTorrent feed films Parser on 1080p");
        return newFilms;
    }

    public List<FilmModelTorrent> getNewShowsMicro4K() {
        List<FilmModelTorrent> newFilms = new ArrayList<>();
        log.info("Ejecutando DonTorrent feed films Parser on 4K");
        String textNewFilms = getHTML(URL_DONTORRENTS_4K, StandardCharsets.UTF_8);
        Document doc = Jsoup.parse(textNewFilms);
        Elements listLi = doc.getElementsByClass("noticiasContent");
        for (Element element : listLi) {
            Elements elementFilms = element.getElementsByClass("text-center");
            for (Element elementFilm : elementFilms) {
                for (Element elementUrl : elementFilm.getElementsByTag("a")) {
                    if (Boolean.TRUE.equals(elementUrl.hasAttr("href"))) {
                        FilmModelTorrent filmModelTorrent = new FilmModelTorrent();
                        filmModelTorrent.setTorrentPage(Page.DON_TORRENT);
                        filmModelTorrent.setTorrentQuality(MICROHD_4K);
                        analizeFilm(URL_DONTORRENTS_DOMAIN + elementUrl.attr("href"), filmModelTorrent);
                        newFilms.add(filmModelTorrent);
                    }
                }
            }
        }
        log.info("Finalizado ejecución DonTorrent feed films Parser on 4K");
        return newFilms;
    }

    private void analizeFilm(String urlFilm, FilmModelTorrent filmModelTorrent) {
        log.info("Analize film by url {}", urlFilm);
        String textNewFilm = getHTML(urlFilm, StandardCharsets.UTF_8);
        Document doc = Jsoup.parse(textNewFilm);
        Elements images = doc.getElementsByClass("img-thumbnail float-left");
        if (Boolean.FALSE.equals(images.isEmpty())) {
            String torrentCroppedTitle = images.first().attr("alt");
            if ("[".contains(torrentCroppedTitle)) {
                torrentCroppedTitle = torrentCroppedTitle.split("\\[")[0].trim();
            }
            filmModelTorrent.setTorrentCroppedTitle(torrentCroppedTitle);
            filmModelTorrent.setTorrentImage(URL_HTTPS + images.first().attr("src"));
        }
        Elements year = doc.getElementsByAttributeValueContaining("onclick", "campo: 'anyo'");
        if (Boolean.FALSE.equals(year.isEmpty()) && Objects.nonNull(NumberUtils.tryParseInt(year.text()))) {
            filmModelTorrent.setTorrentYear(Integer.parseInt(year.text()));
        }

        Elements format = doc.getElementsContainingOwnText("Formato:");
        if (Boolean.FALSE.equals(format.isEmpty())) {
            filmModelTorrent.setTorrentQuality(format.first().parent().text().split("Formato: ")[1]);
        }
        Elements size = doc.getElementsContainingOwnText("Tama");
        if (Boolean.FALSE.equals(size.isEmpty())) {
            filmModelTorrent.setTorrentSize(size.first().parent().text().split(": ")[1]);
        }
        Elements torrentUrl = doc
                .getElementsByClass("text-white bg-primary rounded-pill d-block shadow text-decoration-none p-1");
        if (Boolean.FALSE.equals(torrentUrl.isEmpty())) {
            filmModelTorrent.setTorrentUrl(URL_HTTPS + torrentUrl.first().attr("href"));
        }
        Elements torrentName = doc.getElementsByAttributeValueMatching("name", "description");
        if (Boolean.FALSE.equals(torrentName.isEmpty())) {
            filmModelTorrent.setTorrentTitle(
                    torrentName.first().attr("content").split("cula ")[1].split("torrent gratis en Español")[0]);
        }
        log.info(filmModelTorrent.toString());
    }
}