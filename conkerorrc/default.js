//Default file with basic handling
dumpln("Toto"); //it seems it has never really worked

session_pref("general.useragent.compatMode.firefox", true);


//Learn something new or read !
if (getenv("HOMEPAGE")) {
    homepage = getenv("HOMEPAGE");
} else {
    homepage = "http://en.wikipedia.org/wiki/Special:Random"
}

//External editor
editor_shell_command = "urxvt -e vim";


//Omnibox style for the mini buffer
minibuffer_auto_complete_default = true;
url_completion_use_history = true;

//Favicon everywhere
require("favicon");
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

//----- webjumps -----

//criteo jira
define_webjump("jira","http://agile.criteo/browse/%s");
define_webjump("g","https://encrypted.google.com/search?q=%s");

//----- shortcuts -----

define_key(content_buffer_normal_keymap, "C-t", "find-url-new-buffer");
define_key(content_buffer_normal_keymap, "C-w", "kill-current-buffer");
define_key(content_buffer_normal_keymap, ":", "execute-extended-command");

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

//prevent github and others to steal my keys !
require("key-kill");
key_kill_mode.test.push(build_url_regexp($domain = "github"));
key_kill_mode.test.push(/\/\/.*google\..*\//);
key_kill_mode.test.push(/\/\/.*facebook\..*\//);


// -- youtube experiments
//require("string");
//interactive("open with vlc",
//  "open with vlc",
//  function(I) {
//      var error="";
//      require("string");
//      var result = yield shell_command_with_argument("vlc ", I.buffer.current_uri);
//      //if (result != 0 || error != "")
//      //    throw new interactive_error("status "+result+", "+error);
//  });

const cookie_culler_chrome = "chrome://cookieculler/content/CookieCuller.xul";

interactive("cookie-culler-dialog", "Show the CookieCuller settings in a dialog box.",
    function (I) {
        var frame = I.buffer.top_frame;
        frame.openDialog(cookie_culler_chrome,
                         "CookieCuller",
                         "centerscreen,chrome,dialog,modal,resizable");
    });

interactive("cookie-culler", "Open the CookieCuller settings in a new buffer.",
    "find-url-new-buffer",
    $browser_object = cookie_culler_chrome);
