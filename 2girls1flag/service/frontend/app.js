(() => {
  const BASE = 'http://' + window.location.hostname.split(':')[0] + ':8000';
  let authToken = null;
  let currentUserId = null;
  let state = {
    browser: { which: 'my', page: 1, size: 15, totalPages: 1, loadSeq: 0 },
    girls: [],
    chat: {
      girlId: null,
      character: '',
      girlName: 'Girl',
      sessionId: null,
      lastMessage: '',
      tone: 'NEUTRAL',
    },
  };

  const qs = (s, r = document) => r.querySelector(s);
  const qsa = (s, r = document) => Array.from(r.querySelectorAll(s));
  const on = (el, ev, fn) => { if (el) el.addEventListener(ev, fn); };

  const viewLogin = qs('#view-login');
  const viewMenu = qs('#view-menu');
  const viewBrowser = qs('#view-browser');
  const viewChat = qs('#view-chat');
  const pageIndicator = qs('#page-indicator');
  const girlsGrid = qs('#girls-grid');
  const loginError = qs('#login-error');
  const existingUserInput = qs('#existing-user-id');
  const chatMessage = qs('#chat-message');
  const chatInput = qs('#chat-input');
  const girlNameEl = qs('#girl-name');
  const spriteImg = qs('#sprite-img');
  let waiting = false;
  let typingTimer = null;
  let typingInProgress = false;

  function setView(name) {
    for (const v of [viewLogin, viewMenu, viewBrowser, viewChat]) v.classList.add('hidden');
    if (name === 'login') viewLogin.classList.remove('hidden');
    if (name === 'menu') viewMenu.classList.remove('hidden');
    if (name === 'browser') viewBrowser.classList.remove('hidden');
    if (name === 'chat') viewChat.classList.remove('hidden');
  }

  async function api(path, { method = 'GET', body } = {}) {
    const headers = { 'Content-Type': 'application/json' };
    if (authToken) headers['Authorization'] = `Bearer ${authToken}`;
    const resp = await fetch(BASE + path, { method, headers, cache: 'no-store', body: body ? JSON.stringify(body) : undefined });
    if (!resp.ok) throw new Error(`${resp.status} ${resp.statusText}`);
    return await resp.json();
  }

  async function apiAuth(userId) {
    const d = await api('/auth', { method: 'POST', body: userId ? { user_id: userId } : {} });
    authToken = d.access_token;
    currentUserId = d.user_id;
  }

  async function apiGirls(which, size, page) {
    const q = new URLSearchParams({ size, page });
    q.set('_', Date.now().toString());
    return await api(which === 'my' ? `/girls?${q}` : `/all_girls?${q}`);
  }

  async function apiCreateGirl() {
    return await api('/girls', { method: 'POST' });
  }

  async function apiChat(girlId, message, sessionId) {
    return await api('/chat', { method: 'POST', body: { girl_id: girlId, message, session_id: sessionId } });
  }

  function spritePath(character, tone) {
    const map = {
      'OFFICE_MANAGER': 'manager',
      'ART_STUDENT': 'student',
      'WEALTHY_GIRL': 'wealthy',
    };
    const base = map[(character || '').toUpperCase()];
    if (!base) return null;
    const t = /^(ANTIPATHY|SYMPATHY|NEUTRAL)$/i.test(tone || '') ? tone.toLowerCase() : 'neutral';
    return `/img/sprites/${base}_${t}.png`;
  }

  function updateSprite() {
    const p = spritePath(state.chat.character, state.chat.tone);
    if (p) {
      spriteImg.src = p;
      spriteImg.style.display = '';
    } else {
      spriteImg.removeAttribute('src');
      spriteImg.style.display = 'none';
    }
  }

  function renderGirls(resp) {
    girlsGrid.innerHTML = '';
    state.girls = resp.girls || [];
    state.browser.totalPages = resp.total_pages || 1;
    pageIndicator.textContent = `${state.browser.page} / ${state.browser.totalPages}`;
    for (const g of state.girls) {
      const el = document.createElement('div');
      el.className = 'girl';
      const thumb = document.createElement('div');
      thumb.className = 'thumb';
      const bg = document.createElement('div');
      bg.className = 'bg';
      const img = document.createElement('img');
      const spr = spritePath(g.character, 'NEUTRAL');
      if (spr) img.src = spr;
      const meta = document.createElement('div');
      meta.className = 'meta';
      const name = document.createElement('div');
      name.textContent = g.name || 'Unnamed';
      const btn = document.createElement('button');
      btn.className = 'btn small';
      btn.textContent = 'Select';
      btn.onclick = () => {
        state.chat.girlId = g.girl_id;
        state.chat.character = g.character;
        state.chat.girlName = g.name || 'Unnamed';
        state.chat.sessionId = null;
        state.chat.lastMessage = '';
        state.chat.tone = 'NEUTRAL';
        enterChat();
      };
      thumb.appendChild(bg);
      thumb.appendChild(img);
      meta.appendChild(name);
      meta.appendChild(btn);
      el.appendChild(thumb);
      el.appendChild(meta);
      girlsGrid.appendChild(el);
    }
  }

  function renderChat() {
    girlNameEl.textContent = state.chat.girlName;
    if (waiting) {
      chatMessage.innerHTML = '<span class="typing"><span class="dot"></span><span class="dot"></span><span class="dot"></span></span>';
    } else {
      chatMessage.textContent = state.chat.lastMessage || '';
    }
    updateSprite();
    const inputbar = document.querySelector('.inputbar');
    inputbar.style.display = typingInProgress ? 'none' : 'grid';
  }

  function enterBrowser() {
    setView('browser');
    state.girls = [];
    if (girlsGrid) girlsGrid.innerHTML = '';
    if (pageIndicator) pageIndicator.textContent = '...';
    loadGirls();
  }

  function enterChat() {
    setView('chat');
    state.chat.lastMessage = state.chat.lastMessage || '...';
    renderChat();
  }

  function enterMenu() {
    setView('menu');
  }

  async function loadGirls() {
    const seq = ++state.browser.loadSeq;
    if (girlsGrid) girlsGrid.innerHTML = '';
    try {
      const resp = await apiGirls(state.browser.which, state.browser.size, state.browser.page);
      if (seq !== state.browser.loadSeq) return;
      renderGirls(resp);
    } catch (e) {
      if (seq !== state.browser.loadSeq) return;
      console.error(e);
      if (girlsGrid) girlsGrid.innerHTML = '';
    }
  }

  async function doSend() {
    const msg = chatInput.value.trim();
    if (!msg || msg.length < 2) return;
    if (msg.length > 1000) { chatInput.value = msg.slice(0, 1000); return; }
    chatInput.value = '';
    try {
      typingInProgress = true; renderChat();
      waiting = true; renderChat();
      const cr = await apiChat(state.chat.girlId, msg, state.chat.sessionId);
      state.chat.sessionId = cr.session_id;
      const raw = cr.output_message || '';
      const m = raw.match(/(ANTIPATHY|SYMPATHY|NEUTRAL)\s*$/);
      let cleaned = raw;
      if (m) {
        state.chat.tone = m[1];
        cleaned = raw.replace(/\s*(?:ANTIPATHY|SYMPATHY|NEUTRAL)\s*$/, '');
      } else {
        state.chat.tone = 'NEUTRAL';
        cleaned = raw;
      }
      cleaned = cleaned.trim();
      state.chat.lastMessage = cleaned ? cleaned : '...................';
      waiting = false;
      await typeOut(state.chat.lastMessage, 28);
    } catch (e) {
      waiting = false; state.chat.lastMessage = 'Error: ' + e.message; renderChat();
    }
  }

  async function typeOut(fullText, cps = 28) {
    typingInProgress = true;
    renderChat();
    const delay = 1000 / cps;
    chatMessage.textContent = '';
    let i = 0;
    await new Promise((resolve) => {
      function step() {
        if (i >= fullText.length) { resolve(); return; }
        chatMessage.textContent += fullText[i++];
        chatMessage.scrollTop = chatMessage.scrollHeight;
        typingTimer = setTimeout(step, delay);
      }
      step();
    });
    typingInProgress = false;
    renderChat();
  }

  on(qs('#btn-menu-create'), 'click', async () => {
    try {
      const g = await apiCreateGirl();
      state.chat.girlId = g.girl_id;
      state.chat.character = g.character;
      state.chat.girlName = g.name || 'Unnamed';
      state.chat.sessionId = null;
      state.chat.lastMessage = '';
      state.chat.tone = 'NEUTRAL';
      enterChat();
    } catch (e) { console.error(e); }
  });
  on(qs('#btn-menu-my'), 'click', () => { state.browser.which = 'my'; state.browser.page = 1; enterBrowser(); });
  on(qs('#btn-menu-all'), 'click', () => { state.browser.which = 'all'; state.browser.page = 1; enterBrowser(); });
  on(qs('#btn-create-user'), 'click', async () => {
    loginError.hidden = true;
    try {
      await apiAuth(null);
      enterMenu();
    } catch (e) {
      loginError.hidden = false;
      loginError.textContent = e.message;
    }
  });
  on(qs('#btn-login-existing'), 'click', async () => {
    loginError.hidden = true;
    try {
      await apiAuth(existingUserInput.value.trim() || null);
      enterMenu();
    } catch (e) {
      loginError.hidden = false;
      loginError.textContent = e.message;
    }
  });
  
  on(qs('#btn-prev'), 'click', () => { if (state.browser.page > 1) { state.browser.page--; loadGirls(); } });
  on(qs('#btn-next'), 'click', () => { if (state.browser.page < state.browser.totalPages) { state.browser.page++; loadGirls(); } });
  on(qs('#btn-send'), 'click', doSend);
  on(chatInput, 'keydown', (e) => { if (e.key === 'Enter') doSend(); });
  on(qs('#btn-back-browser'), 'click', () => enterMenu());
  on(qs('#btn-back-chat'), 'click', () => enterMenu());

  on(document, 'keydown', (e) => {
    if (e.key !== 'Escape') return;
    if (!viewLogin.classList.contains('hidden')) return;
    enterMenu();
  });

  function init() {
    setView('login');
  }

  init();
})();


