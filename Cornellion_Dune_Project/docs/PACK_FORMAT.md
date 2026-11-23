# Question pack format

Owner: Gavin (gl568). Parser: `lib/pack.ml`. Default pack:
`data/default_pack.txt`.

A pack is a plain text file. Each line is either blank, a comment, or one
`key: value` directive. Leading and trailing spaces are ignored.

```
# Lines starting with # are comments.
pack: Cornell Basics

question: dining-hall
prompt: Name a Cornell dining hall
answer: Okenshields | 180 | Okies, Okenshield's
answer: 104West! | 12 | 104 West
```

## Directives

| Key | Value | Rules |
| --- | --- | --- |
| `pack:` | Pack name | Optional. At most once, before the first question. Defaults to `Untitled pack`. |
| `question:` | Question id | Starts a new question. Lowercase letters, digits, and hyphens only. Unique in the pack. |
| `prompt:` | Text shown to the player | Exactly one per question, after its `question:` line. |
| `answer:` | `Name \| count \| nickname, nickname` | At least one per question. Name and count are required; nicknames are optional. |

## Answers

`answer: Name | count | nickname1, nickname2`

- **Name** is what the game shows as the understood answer. It cannot contain `|`.
- **count** is the simulated number of players who gave this answer. It must be a
  whole number of at least 1. Lower counts mean rarer answers and more points.
- **Nicknames** are other spellings the game should accept, separated by commas.
  They cannot contain `|` or `,`. Blank entries are skipped, and a nickname that
  repeats its own answer's name is dropped.

Within one question, no name or nickname may be used by two different answers.
The check ignores capital letters and surrounding spaces. For example, `Okies`
cannot be a nickname for both Okenshields and another dining hall. This keeps a
typed answer from matching two answers at once.

## Errors

Loading stops at the first problem and reports its line number, for example:

```
Line 7: count "lots" is not a whole number
Line 12: "becker" is already used by answer "Becker House" on line 10
Line 3: question "dining-hall" has no answers
```

Problems that are not tied to one line, such as an unreadable file or a pack
with no questions, are reported without a line number.
