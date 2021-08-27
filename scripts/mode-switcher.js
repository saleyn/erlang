let preferDark = window.matchMedia("(prefers-color-scheme: dark)");
let theme      = sessionStorage.getItem('theme');

document.getElementById("theme-toggle").innerHTML = preferDark.matches
                                                  ? "Light Mode"
                                                  : "Dark Mode";
function switchMode(isLight, theme) {
  let mode = isLight ? 'dark'       : 'light';
  let name = isLight ? 'Light Mode' : 'Dark Mode';
  sessionStorage.setItem('theme', theme === '' ? theme : mode);
  document.documentElement.setAttribute("theme-toggle",  mode);
  document.getElementById("theme-toggle").innerHTML   =  name;
}

function prefersColorTest(preferDark) {
  switchMode(preferDark.matches, '');
}
preferDark.addListener(prefersColorTest);

function modeSwitcher() {
	let theme   = sessionStorage.getItem('theme');
  let isLight = theme === 'light' || !preferDark.matches;
  switchMode(isLight, theme);
}

switchMode(theme === 'light', theme);
