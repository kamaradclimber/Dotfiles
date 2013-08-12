//Default file with basic handling
dumpln("Toto"); //it seems it has never really worked


//Learn something new !
homepage = "http://en.wikipedia.org/wiki/Special:Random"

//External editor
editor_shell_command = "urxvt -e vim";


//Omnibox style for the mini buffer
minibuffer_auto_complete_default = true;
url_completion_use_history = true;

//----- webjumps -----

//criteo jira
define_webjump("jira","http://agile.criteo/browse/%s");

//----- shortcuts -----

define_key(default_global_keymap,'/','isearch-forward')
define_key(content_buffer_normal_keymap, "C-t", "find-url-new-buffer");
define_key(content_buffer_normal_keymap, "C-w", "kill-current-buffer");

//------ Bindings test -----
//function scroll_verti_complete (buffer, n) {
//    vqr w = buffer.focused_frame;
//    w.scrollTo (n > 0 ? w.scrollMaxY : 0, w.scrollX);
//}
//interactive("scroll-end-of-page",
//    "Scroll to the end the page",
//    function (I) { scroll_verti_complete(I.buffer, 1 });
//define_key("default_global_keymap", "H", "scroll-end-of-age")


//----- Page modes -----
xkcd_add_title = true;

require("youtube"); //I should set a vlc handler instead

require("smbc");
