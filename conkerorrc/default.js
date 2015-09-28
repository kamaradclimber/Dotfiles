session_pref("general.useragent.compatMode.firefox", true);

url_completion_use_bookmarks = true;

//Activate http pipelining.
session_pref("network.http.pipelining", true);

// I am weak and want to use mouse
require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now buffers open in background.

//Learn something new or read !
if (getenv("HOMEPAGE")) {
    homepage = getenv("HOMEPAGE");
    dumpln("using " + homepage + " as homepage");
} else {
    homepage = "http://en.wikipedia.org/wiki/Special:Random"
}

//External editor
editor_shell_command = "urxvt -e vim --nofork";
view_source_use_external_editor = true;


//Omnibox style for the mini buffer
minibuffer_auto_complete_default = true;
url_completion_use_history = true;

//Favicon everywhere
require("favicon");
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

//----- webjumps -----

//criteo jira
define_webjump("j","https://jira.criteois.com/browse/%s");

//criteo gitlab
define_webjump("gitlab","https://gitlab.criteois.com/search?utf8=%E2%9C%93&search=%s");

//confluence search
define_webjump("c", "https://confluence.criteois.com/dosearchsite.action?queryString=%s");

//google search
define_webjump("g","https://encrypted.google.com/search?q=%s");

// elasticsearch doc
define_webjump("el","https://www.elastic.co/search?q=%s");

//----- shortcuts -----

define_key(content_buffer_normal_keymap, "C-t", "find-url-new-buffer");
define_key(content_buffer_normal_keymap, "C-w", "kill-current-buffer");
define_key(content_buffer_normal_keymap, ":", "execute-extended-command");
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer");

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
//page_mode_deactivate(key_kill_mode)
//page_mode_activate(key_kill_mode)

//key_kill_mode.test.push(build_url_regexp($domain = "github", $allow_www = true));
//require('github');
//page_mode_deactivate(github_mode)
//key_kill_mode.test.push(/\/\/.*google\..*\//);
//key_kill_mode.test.push(/\/\/.*facebook\..*\//);


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

// last pass bookmarklet
define_webjump("login-lastpass", "javascript:((function(){/*AutoFill_LastPass*/_LPG=function(i){return%20document.getElementById(i);};_LPT=function(i){return%20document.getElementsByTagName(i);};if(_LPG('_lpiframe')){_LPG('_lpiframe').parentNode.removeChild(_LPG('_lpiframe'));}if(_LPG('_LP_RANDIFRAME')){_LPG('_LP_RANDIFRAME').parentNode.removeChild(_LPG('_LP_RANDIFRAME'));}_LASTPASS_INC=function(u,s){if(u.match(/_LASTPASS_RAND/)){alert('Cancelling_request_may_contain_randkey');return;}s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src',u);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(s);}else{_LPT('head').item(0).appendChild(s);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(s);}else{_LPT('body').item(0).appendChild(s);}}};_LASTPASS_INC('https://lastpass.com/bml.php'+String.fromCharCode(63)+'v=0&a=0&r='+Math.random()+'&h=56ceff1ec4c8f883832eab281ce4a24b76ef5935e57fcbde42170adfd95ee028&u='+escape(document.location.href));_LPM=function(m){var%20targetFrame=_LPG(m.data.frame);if(null!=targetFrame&&typeof(targetFrame)!='undefined'&&typeof(targetFrame.contentWindow)!='undefined')targetFrame.contentWindow.postMessage(m.data,'*');};if(window.addEventListener){window.addEventListener('message',_LPM,false);}else{window.attachEvent('onmessage',_LPM);}var%20t=document.createElement('iframe');t.setAttribute('id','_LP_RANDIFRAME');t.setAttribute('sandbox','allow-scripts');t.frameBorder='0';t.setAttribute('src','https://lastpass.com/bml.php?u=1&hash=1&gettoken=0&donotcache=1436165360987426341');t.setAttribute('onload',\"document.getElementById('_LP_RANDIFRAME').contentWindow.postMessage('3847d44271184be49603797436e56a47855d7301fedbcb4e55324729b9dfba63','*');\");if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(t);}else{document.getElementByTagName('head').item(0).appendChild(t);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(t);}else{_LPT('body').item(0).appendChild(t);}}})());")


// undo close buffer feature
define_key(default_global_keymap, "C-T", "revive-buffer");

var kill_buffer_original = kill_buffer_original || kill_buffer;

var killed_buffer_urls = [];
var killed_buffer_histories = [];

//  remember_killed_buffer
kill_buffer = function (buffer, force) {
    var hist = buffer.web_navigation.sessionHistory;

    if (buffer.display_uri_string && hist) {
        killed_buffer_histories.push(hist);
        killed_buffer_urls.push(buffer.display_uri_string);
    }

    kill_buffer_original(buffer,force);
};

interactive("revive-buffer",
    "Loads url from a previously killed buffer",
    function restore_killed_buffer (I) {
        if (killed_buffer_urls.length !== 0) {
            var url = yield I.minibuffer.read(
                $prompt = "Restore killed url:",
                $completer = new all_word_completer($completions = killed_buffer_urls),
                $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
                $auto_complete = "url",
                $auto_complete_initial = true,
                $auto_complete_delay = 0,
                $require_match = true);

            var window = I.window;
            var creator = buffer_creator(content_buffer);
            var idx = killed_buffer_urls.indexOf(url);

            // Create the buffer
            var buf = creator(window, null);

            // Recover the history
            buf.web_navigation.sessionHistory = killed_buffer_histories[idx];

            // This line may seem redundant, but it's necessary.
            var original_index = buf.web_navigation.sessionHistory.index;
            buf.web_navigation.gotoIndex(original_index);

            // Focus the new tab
            window.buffers.current = buf;

            // Remove revived from cemitery
            killed_buffer_urls.splice(idx,1);
            killed_buffer_histories.splice(idx,1);
        } else {
            I.window.minibuffer.message("No killed buffer urls");
        }
    });

// bigger hint numbers
register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
            "span.__conkeror_hint {\n"+
            "  font-size: 18px !important;\n"+
            "  line-height: 18px !important;\n"+
            "}"));

function darken_page (I) {
    var styles='* { background: black !important; color: grey !important; }'+
        ':link, :link * { color: #4986dd !important; }'+
        ':visited, :visited * { color: #d75047 !important; }';
    var document = I.buffer.document;
    var newSS=document.createElement('link');
    newSS.rel='stylesheet';
    newSS.href='data:text/css,'+escape(styles);
    document.getElementsByTagName("head")[0].appendChild(newSS);
}
interactive("darken-page", "Darken the page in an attempt to save your eyes.",
            darken_page);

define_webjump("couchpotato", "javascript:void((function(){var%20e=document.createElement('script');e.setAttribute('type','text/javascript');e.setAttribute('charset','UTF-8');e.setAttribute('src','http://capodimonte/couchpotato/api/1a259810e5c94c9ca29098ce358d004e/userscript.bookmark/?host=http://capodimonte/couchpotato/api/1a259810e5c94c9ca29098ce358d004e/userscript.get/LuPmoRwi/&r='+Math.random()*99999999);document.body.appendChild(e)})());");
