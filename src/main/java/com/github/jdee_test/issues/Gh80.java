package com.github.jdee_test.issues;

import java.util.regex.Pattern;

/**
 * @see <a href="https://github.com/jdee-emacs/jdee/issues/80">gh-80</a>
 */

public class Gh80 {
    final static private Pattern pattFlName = Pattern.compile("([-'A-Za-z .]+)[ ,;]+([-'A-Za-z]+)$");
    final static private Pattern pattLfName = Pattern.compile("^([-'A-Za-z]+)[ ,;]+([-'A-Za-z .]+)");
}