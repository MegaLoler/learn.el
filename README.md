# learn.el

Just some stuff for drilling over items in a tab-separated-values (`.tsv`) file.
Plays audio!

Learning sessions are prompted for a mode (flash, query, multi):
* Flash mode simply presents you with cues. When you press enter, it will show yo
u the recall item. Then you answer whether recalled it successfully.
* Query mode is like flash mode but you are required to type the recall item in r
esponse.
* Multi mode presents you with a set of choices, one of which is correct, and you
 pick one.

Additionally, learning sessions are prompted for fields from the `.tsv` to use as cues, and fields to use as recall items. Then you can provide an elisp expression to filter out all of the items that you'd like to learn in this session, and another elisp expression to sort the items (for example, to scramble them).

## Commands

`M-x learn-from-file`
This will prompt you to open a .tsv file and it will drill you.

`M-x learn-from-buffer`
This is like the above, but it will read from an emacs buffer instead.

`M-x learn-again`
Repeat the previous learning session.

`M-x review-missed`
Start a session with the items you missed in the previous session.

`C-c C-r`
Replay the last audio played.
