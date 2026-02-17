/* ============================================
   EXPOUND TECHNIVO — PROFESSIONAL WEBSITE SCRIPTS
   ============================================ */

document.addEventListener('DOMContentLoaded', () => {

    // ============ NAVBAR ============
    const navbar = document.getElementById('navbar');
    const navLinks = document.querySelectorAll('.nav-link');
    const sections = document.querySelectorAll('section[id]');

    function handleNavScroll() {
        navbar.classList.toggle('scrolled', window.scrollY > 60);

        // Active section highlighting
        let current = '';
        sections.forEach(section => {
            const sectionTop = section.offsetTop - 120;
            if (window.scrollY >= sectionTop) {
                current = section.getAttribute('id');
            }
        });

        navLinks.forEach(link => {
            link.classList.remove('active');
            if (link.getAttribute('href') === `#${current}`) {
                link.classList.add('active');
            }
        });
    }

    window.addEventListener('scroll', handleNavScroll, { passive: true });
    handleNavScroll();

    // ============ MOBILE MENU ============
    const hamburger = document.getElementById('hamburger');
    const navLinksContainer = document.getElementById('navLinks');

    hamburger.addEventListener('click', () => {
        const isActive = hamburger.classList.toggle('active');
        navLinksContainer.classList.toggle('active');
        document.body.style.overflow = isActive ? 'hidden' : '';
    });

    navLinksContainer.querySelectorAll('.nav-link').forEach(link => {
        link.addEventListener('click', () => {
            hamburger.classList.remove('active');
            navLinksContainer.classList.remove('active');
            document.body.style.overflow = '';
        });
    });

    // ============ SCROLL ANIMATIONS ============
    const animateElements = document.querySelectorAll('[data-animate]');

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                // Stagger siblings
                const parent = entry.target.parentElement;
                const siblings = Array.from(parent.querySelectorAll('[data-animate]'));
                const index = siblings.indexOf(entry.target);
                const delay = index * 80;

                setTimeout(() => {
                    entry.target.classList.add('animate-in');
                }, delay);

                observer.unobserve(entry.target);
            }
        });
    }, {
        threshold: 0.08,
        rootMargin: '0px 0px -40px 0px'
    });

    animateElements.forEach(el => observer.observe(el));

    // ============ COUNTER ANIMATION ============
    const counters = document.querySelectorAll('[data-target]');

    const counterObserver = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const target = parseInt(entry.target.getAttribute('data-target'));
                animateValue(entry.target, 0, target, 1800);
                counterObserver.unobserve(entry.target);
            }
        });
    }, { threshold: 0.5 });

    counters.forEach(el => counterObserver.observe(el));

    function animateValue(el, start, end, duration) {
        const startTime = performance.now();

        function update(currentTime) {
            const elapsed = currentTime - startTime;
            const progress = Math.min(elapsed / duration, 1);
            // Ease-out quart
            const eased = 1 - Math.pow(1 - progress, 4);
            el.textContent = Math.round(start + (end - start) * eased);

            if (progress < 1) {
                requestAnimationFrame(update);
            }
        }

        requestAnimationFrame(update);
    }

    // ============ SMOOTH SCROLL ============
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            const id = this.getAttribute('href');
            if (id === '#') return;

            const target = document.querySelector(id);
            if (target) {
                const offset = target.offsetTop - 80;
                window.scrollTo({ top: offset, behavior: 'smooth' });
            }
        });
    });

    // ============ CONTACT FORM ============
    const form = document.getElementById('contactForm');

    form.addEventListener('submit', function (e) {
        e.preventDefault();

        const btn = this.querySelector('button[type="submit"]');
        const originalHTML = btn.innerHTML;

        // Loading state
        btn.innerHTML = '<span>Sending...</span><div class="spinner"></div>';
        btn.disabled = true;
        btn.style.opacity = '0.7';

        // Simulate send
        setTimeout(() => {
            btn.innerHTML = '<span>Message Sent Successfully!</span><svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="20 6 9 17 4 12"/></svg>';
            btn.style.background = '#16a34a';
            btn.style.opacity = '1';

            setTimeout(() => {
                form.reset();
                btn.innerHTML = originalHTML;
                btn.disabled = false;
                btn.style.background = '';
            }, 3000);
        }, 1500);
    });

    // Add spinner styles
    const style = document.createElement('style');
    style.textContent = `
        .spinner {
            width: 18px; height: 18px;
            border: 2px solid rgba(255,255,255,0.3);
            border-top-color: white;
            border-radius: 50%;
            animation: spin 0.6s linear infinite;
        }
        @keyframes spin { to { transform: rotate(360deg); } }
    `;
    document.head.appendChild(style);

});
