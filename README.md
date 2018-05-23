langtool.el
===========

## Installation

Install [LanguageTool](http://www.languagetool.org/) (and Java)

Put this file into load-path'ed directory, and byte compile it if desired.
Then insert the following expression into your Emacs initialization file:

```
(require 'langtool)
(setq langtool-language-tool-jar "/path/to/languagetool-commandline.jar")
```

If you use old version of LanguageTool then you need to reference the old JAR name:

```
(setq langtool-language-tool-jar "/path/to/LanguageTool.jar")
```

Alternatively, you can use LanguageTool via the `CLASSPATH` by adding its
JAR directory like this:

```
(require 'langtool)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
```

## Configuration

### Key Bindings

There aren't any made in this package but the following work well:

```
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)
```

### Default Language

The default language is detected by the `LANG/LC_ALL` environment variable.
If you need a different value then set `langtool-default-language` like this:

```
(setq langtool-default-language "en-US")
```

Alternatively choose a language a single session by invoking `M-x
langtool-check` with `C-u` (universal-argument)

### Mother Tougne

Your mother tougne is different than the default language.

Maybe you want to specify your mother tongue:

```
(setq langtool-mother-tongue "en")
```

## Java Distribution

Currently LanguageTool doesn't work with GNU Java. Please change the variable to your
favorite Java executable.

Choose your Java executable like this:
```
(setq langtool-java-bin "/path/to/java")
```

**FYI**: As of 2018-05-23 LanguageTool doesn't work with GNU Java.

## Using The LanguageTool Launcher

In addition to executing LanguageTool directory from Java, LanguageTool also
comes with a script run it. This is referred to as it's *launcher*. Your
LanguageTool installation *might* have one installed.

It it does and you want to use it then you need to set `langtool-bin`:

```
(setq langtool-bin "/usr/bin/languagetool")
```

See https://github.com/mhayashi1120/Emacs-langtool/issues/24 for details.

### Command Line & User Arguments

To customize LanguageTool commandline arguments:

```
(setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
```

You can also make the variable to buffer local like following:

```
(add-hook '**SOME**-mode-hook
          (lambda () (set (make-local-variable 'langtool-java-user-arguments)
                         '("-Dfile.encoding=UTF-8"))))
```

**NOTE**: Although there is no good example, `langtool-user-arguments` is a
 similar custom variable.

### Show LanguageTool Report Rutomatically by `popup`

Show LanguageTool's report automatically using `popup`

This idea come from http://d.hatena.ne.jp/LaclefYoshi/20150912/langtool_popup.

```
(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
```

```
(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)
```

## Usage

<table>

<thead>
<tr>
<th>Feature</th>
<th>Command</th>
</tr>
</thead>
<tbody>
<tr>
<td>Check Buffer And Show Warnings</td>
<td><i>langtool-check</i></td>
</tr>
<tr>
<td>Choose Language, Check Buffer, And Show Warnings</td>
<td><i>C-u langtool-check</i></td>
</tr>
<tr>
<td>Visit Warnings And Maybe Correct Them </td>
<td><i>langtool-correct-buffer</i></td>
</tr>
<tr>
<td>Show Warning At Point </td>
<td><i>llangtool-show-message-at-point</i></td>
</tr>
<tr>
<td>Finish Session </td>
<td><i>langtool-check-done</i></td>
</tr>

</tbody>
</table>
