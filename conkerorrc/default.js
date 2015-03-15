//Default file with basic handling
dumpln("Toto"); //it seems it has never really worked

session_pref("general.useragent.compatMode.firefox", true);

//Activate http pipelining.
session_pref("network.http.pipelining", true);


//Learn something new or read !
if (getenv("HOMEPAGE")) {
    homepage = getenv("HOMEPAGE");
    dumpln("using " + homepage + " as homepage");
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
define_webjump("jira","https://jira.criteois.com/browse/%s");

//confluence search
define_webjump("c", "https://confluence.criteois.com/dosearchsite.action?queryString=%s");

//google search
define_webjump("g","https://encrypted.google.com/search?q=%s");

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

define_webjump("login-lastpass", "javascript:((function(){/*AutoFill_LastPass*/_LPG=function(i){return%20document.getElementById(i);};_LPT=function(i){return%20document.getElementsByTagName(i);};if(_LPG('_lpiframe')){_LPG('_lpiframe').parentNode.removeChild(_LPG('_lpiframe'));}if(_LPG('_LP_RANDIFRAME')){_LPG('_LP_RANDIFRAME').parentNode.removeChild(_LPG('_LP_RANDIFRAME'));}_LASTPASS_INC=function(u,s){if(u.match(/_LASTPASS_RAND/)){alert('Cancelling_request_may_contain_randkey');return;}s=document.createElement('script');s.setAttribute('type','text/javascript');s.setAttribute('src',u);if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(s);}else{_LPT('head').item(0).appendChild(s);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(s);}else{_LPT('body').item(0).appendChild(s);}}};_LASTPASS_INC('https://lastpass.com/bml.php'+String.fromCharCode(63)+'v=0&a=0&r='+Math.random()+'&h=a404ad37cf52109e9f15cd37dfed3d6fccbd67d94232e03691fd04891e23148c&u='+escape(document.location.href));_LPM=function(m){var%20targetFrame=_LPG(m.data.frame);if(null!=targetFrame&&typeof(targetFrame)!='undefined'&&typeof(targetFrame.contentWindow)!='undefined')targetFrame.contentWindow.postMessage(m.data,'*');};if(window.addEventListener){window.addEventListener('message',_LPM,false);}else{window.attachEvent('onmessage',_LPM);}var%20t=document.createElement('iframe');t.setAttribute('id','_LP_RANDIFRAME');t.setAttribute('sandbox','allow-scripts');t.frameBorder='0';t.setAttribute('src','https://lastpass.com/bml.php?u=1&hash=1&gettoken=0&donotcache=1417975644624095295');t.setAttribute('onload',\"document.getElementById('_LP_RANDIFRAME').contentWindow.postMessage('7d9e4ca65b417f6cd54d73ce87d565bf50a9af4a813ad8bdb9e041869daf9a9f','*');\");if(typeof(window.attachEvent)!='undefined'){if(_LPT('body').length){_LPT('body').item(0).appendChild(t);}else{document.getElementByTagName('head').item(0).appendChild(t);}}else{if(_LPT('head').length){_LPT('head').item(0).appendChild(t);}else{_LPT('body').item(0).appendChild(t);}}})());")
