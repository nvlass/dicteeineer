# Dicteeineer

Static re-frame/shadow-cljs SPA for pitch and melodic dictation, no backend. Sibling of `../chordtrainer` and `../counterpointer` (same stack; `audio`, `midi`, `storage`, `i18n` and the samples were copied from counterpointer). See README for modes and deploy.

## Commands

- `npx shadow-cljs watch app`: dev server on :8282
- `npx shadow-cljs compile test`: runs `test/` on node
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose (deploy by `git pull`). Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- Notes are `{:s step :a alt}` as in counterpointer (C4 = 28). Keys are `{:tonic {:l :a} :mode :major|:minor}`; minor melodies use harmonic minor (`t/scale-alt`) while the signature is natural minor (`t/sig-alt`). Notation follows bar accidentals: `t/accidentals` decides what to print, `t/implied-alt` what an unmarked note means.
- `theory.cljs`, `rhythm.cljs` and `melody.cljs` are pure. `melody/generate` walks scale degrees, filtering moves through `melodic-ok?` (no augmented/diminished leaps, no 7ths) while it builds, and retries when it gets stuck. The test `melodies` runs every key × clef × range × chromatic; run it after touching the generator.
- Rhythm: durations are ticks, 12 per quarter (`rh/ticks`), so triplet eighths (4) and sixteenths (3) are integers. A note event is `{:n note :v value :dot :trip}`; a bar is a vector of events, no rests or ties yet. `rh/cells` lists the rhythm cells per level and where they may start; `melody/generate` picks cells per bar, then lays the pitch walk over them. Grading (`rh/grade-bar`) matches notes by onset.
- Dictation state is `:dict` in the db (`:version` 2; older saved dictations are dropped on load): `:melody` and `:answer` are vectors of bars, `:bar` the current bar, `:sel` the cursor index in it (= count means append), `:dur` the input duration, `:checked`/`:graded` the checked bars and their results. Only the current, unchecked bar is editable (`events/editable?`).
- Playback highlights carry `[src bar i]`; for an unchecked bar only the bar is highlighted, so the rhythm isn't given away.
- Hiccup: never return a vector of vectors as children (Reagent calls the first one as a component); wrap in `[:g ...]` or return a seq.
- Don't pass a map with a `:key` entry as a component's first argument: Reagent uses it as the React key. That's why score props call the key signature `:keysig` and the dictation components subscribe to `:dict` themselves.
- Don't destructure a key named `range` in fns that also call `range`.
- Persistence: IndexedDB via idb-keyval, keys `settings`, `attempts` (note mode), `history` (finished dictations), `dict` (dictation in progress), as EDN strings. New setting keys need a default in `events/default-settings`.
- Timers carry a `:token`; any new delayed event must check it, or carry the bar it was meant for (`[:dict/play bar]`).
- `resources/public/sw.js` lists the app shell and samples; bump `VERSION` when the cached set changes.

## Roadmap

`TODO.md`.
