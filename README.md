# Dicteeineer

Ear training for pitch and melodic dictation (dictée + engineer). Sibling of `../chordtrainer` and `../counterpointer`: static re-frame SPA, works offline (PWA), English and Greek, everything stays in the browser (IndexedDB).

## Modes

- **Note**: a single note plays; find it on the on-screen piano (or a MIDI keyboard). One octave (the clef's middle octave: C4–B4 treble, C3–B3 bass) or three (C3–B5, shown on the grand staff). White keys only, or all 12 notes with accidentals. An optional reference note (C4 or A4) plays before the first note and stays on a button; without it this is absolute-pitch practice. After answering, the note is shown on the staff and you can hear yours against it.
- **Dictation**: a generated melody in 4/4 or 3/4, 4, 8 or 12 bars, in any major or (harmonic) minor key. The scale plays up and down once, then each bar is played together with the one before it. Write the bar, check it (wrong notes are marked and the right ones shown), and move on. **Strict** allows 2 plays per bar, **free** any number (plus the scale on demand). Optional chromatic notes: half-step approaches to the next note, raised going up and lowered going down. The first note can be given, and an optional count-in plays a bar of clicks before each play.

Rhythm levels (each adds to the ones before):

1. Quarter notes only.
2. Half, dotted half and whole notes (the melody then ends on a long note).
3. Pairs of eighths.
4. Dotted quarter + eighth.
5. Sixteenths: four, eighth + two, two + eighth, dotted eighth + sixteenth.
6. Eighth-note triplets.

Checked bars mark each note: green right, orange right pitch spelled differently, blue right pitch at the wrong time or length, red wrong pitch; the correct notes are shown faintly behind yours. The result gives pitch and rhythm scores separately.

Input in dictation: pick a duration with the buttons or `1`–`5` (sixteenth to whole), `.` for a dot and `T` for a triplet (the next three notes), then enter pitches by clicking the staff, on the on-screen piano, a MIDI keyboard, or the computer keyboard (`A`–`G` enter a note near the previous one, `↑`/`↓` move it, `#` `-` `n` accidentals, `x` respell, `←`/`→` select, `⌫` delete, `Space` play, `Enter` check / next). With a note selected, a duration or dot changes it. Accidentals typed right after a letter apply to that note. Notes that would overflow the bar are refused (no ties yet). A note at the right pitch but spelled differently counts as right, marked in orange.

Clef (treble, bass, grand staff) and range (narrow, about an octave; wide, with ledger lines) are global settings and decide where melodies go:

| Clef | Narrow | Wide |
|---|---|---|
| Treble | C4–C5 | A3–C6 |
| Bass | C3–C4 | C2–E4 |
| Grand staff | C3–C6 | C2–C6 |

Stats: accuracy per note, per octave and the most frequent confusions for the note mode; a list of finished dictations with their scores.

## Develop

```sh
npm install
npx shadow-cljs watch app     # http://localhost:8282
npx shadow-cljs compile test  # theory, melody and grading tests (node)
```

## Build & deploy

```sh
npm run dist
rsync -a --delete dist/ server:/var/www/dicteeineer/
```

Plain static files; the nginx config from chordtrainer's README applies as is (no-cache for `sw.js`, `index.html`, `/js/`; immutable for `/samples/`).

Piano samples: FluidR3_GM acoustic grand (via [gleitz/midi-js-soundfonts](https://github.com/gleitz/midi-js-soundfonts), CC BY 3.0).
