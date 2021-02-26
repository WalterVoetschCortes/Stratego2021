
/*===== ACTIVATE AND REMOVE MENU =====*/
const navLink = document.querySelectorAll('.nav__link')

function linkAction() {
    // active link
    navLink.forEach(n => n.classList.remove('active'))
    this.classList.add('active')

    // remove menu mobile
    const navMenu = document.getElementById('nav-menu')
    navMenu.classList.remove('show')
}

navLink.forEach(n => n.addEventListener('click', linkAction))


/*===== CHANGE MENU SCROLLING =====*/

const options = {
    threshold: 0.3,
};

const addActiveClass = (entries, observer) => {
    entries.forEach((entry) => {
        if(entry.isIntersecting && entry.intersectionRatio >= 0.2){
            console.log(entry.target);
            let currentActive = document.querySelector('.active');

            if(currentActive){
                currentActive.classList.remove('active');
            }

            let newActive = document.querySelector(
                `a[href="#${entry.target.getAttribute("id")}"]`
            );

            newActive.classList.add('active');
        }
    });
};

const observer = new IntersectionObserver(addActiveClass, options);

const sections = document.querySelectorAll("section");

sections.forEach((section) => {
    observer.observe(section);
});
