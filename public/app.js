function setTheme() {
    const prefersDarkScheme = window.matchMedia("(prefers-color-scheme: dark)").matches;
    const theme = prefersDarkScheme ? "dark" : "light";
    document.documentElement.setAttribute("data-bs-theme", theme);
}

setTheme();

window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", setTheme);
