const {
  aceVimMap,
  mapkey,
  imap,
  iunmap,
  imapkey,
  getClickableElements,
  vmapkey,
  map,
  unmap,
  cmap,
  addSearchAlias,
  removeSearchAlias,
  tabOpenLink,
  readText,
  Clipboard,
  Front,
  Hints,
  Visual,
  RUNTIME,
} = api;

// ---- Settings ----
Hints.setCharacters('asdfgyuiowertnm');
settings.scrollStepSize = 140;
settings.hintAlign = 'left';
// settings.aceKeybindings = 'emacs'
settings.nextLinkRegex = /((forward|>>|next|次[のへ]|→)+)/i;
settings.prevLinkRegex = /((back|<<|prev(ious)?|前[のへ]|←)+)/i;
settings.historyMUOrder = false;
settings.theme = `
#sk_status, #sk_find {
  font-size: 12pt;
}
#sk_omnibarSearchResult .highlight {
  background: #f9ec89;
}
`;

// ---- Utils ----
const unmapKeys = (keys) => keys.forEach((key) => unmap(key));
const iunmapKeys = (keys) => keys.forEach((key) => iunmap(key));
const escapeMap = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;',
  '"': '&quot;',
  "'": '&#39;',
  '/': '&#x2F;',
  '`': '&#x60;',
  '=': '&#x3D;',
};
const escapeForAlias = (str) =>
  String(str).replace(/[&<>"'`=/]/g, (s) => escapeMap[s]);
const createSuggestionItem = (html, props = {}) => {
  const li = document.createElement('li');
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};
const padZero = (txt) => `0${txt}`.slice(-2);
const formatDate = (date, format = 'YYYY/MM/DD hh:mm:ss') =>
  format
    .replace('YYYY', date.getFullYear())
    .replace('MM', padZero(date.getMonth() + 1))
    .replace('DD', padZero(date.getDate()))
    .replace('hh', padZero(date.getHours()))
    .replace('mm', padZero(date.getMinutes()))
    .replace('ss', padZero(date.getSeconds()));

const sendArrowKey = (direction) => {
  const keyCodes = {
    ArrowUp: 38,
    ArrowDown: 40,
    ArrowLeft: 37,
    ArrowRight: 39,
  };

  const target = document.activeElement || document.body;
  target.dispatchEvent(
    new KeyboardEvent('keydown', {
      key: direction,
      code: direction,
      keyCode: keyCodes[direction],
      bubbles: true,
    }),
  );
};

const tabOpenBackground = (url) =>
  RUNTIME('openLink', {
    tab: {
      tabbed: true,
      active: false,
    },
    url,
  });

// ---- Maps ----
map('>_r', 'r');
map('>_E', 'E');
map('>_R', 'R');
map('>_S', 'S');
map('S', 'sg'); // 選択したテキストまたはクリップボードからググる
map('r', 'gf'); // 別タブで開く
map('H', '>_S'); // back in history
map('L', 'D'); // forward in history
map('h', 'E'); // previousTab
map('l', '>_R'); // nextTab
map('R', '>_r'); // reload

map('{', '[[');
map('}', ']]');
mapkey('[', '#1Send ArrowLeft', () => sendArrowKey('ArrowLeft'));
mapkey(']', '#1Send ArrowRight', () => sendArrowKey('ArrowRight'));

map('<Ctrl-[>', '<Esc>');

iunmap(':'); // disable emoji
// disable vim binding in insert mode
iunmapKeys([
  '<Ctrl-a>',
  '<Ctrl-e>',
  '<Ctrl-f>',
  '<Ctrl-b>',
  '<Ctrl-k>',
  '<Ctrl-y>',
]);
// disable proxy
unmapKeys([';cp', ';pa', ';pb', ';pc', ';pd', ';ps', ';ap']);

// ---- Search ----
removeSearchAlias('b');
removeSearchAlias('w');

// hatena tag
addSearchAlias(
  'th',
  'hatena tag',
  'https://b.hatena.ne.jp/q/{0}?safe=on&sort=recent&date_range=5y&target=tag&users=3',
);

// Twitter
addSearchAlias(
  'x',
  'Twitter',
  'https://x.com/search?q=',
  's',
  'https://x.com/i/search/typeahead.json?count=10&filters=true&q=',
  (response) =>
    JSON.parse(response.text).topics.map((v) =>
      createSuggestionItem(v.topic, {
        url: `https://x.com/search?q=${encodeURIComponent(v.topic)}`,
      }),
    ),
);
mapkey('otw', '#8Open Search with alias tw', function () {
  Front.openOmnibar({ type: 'SearchEngine', extra: 'tw' });
});

addSearchAlias('xf', 'Twitter フォロワーのみ', 'https://x.com/search?pf=on&q=');

// Google jp 3ヶ月以内
addSearchAlias(
  '3',
  'Google 3ヶ月以内',
  'https://www.google.co.jp/search?q={0}&tbs=qdr:m3,lr:lang_1ja&lr=lang_ja',
);

// Yahoo!リアルタイム検索
addSearchAlias(
  'r',
  'Yahoo!リアルタイム検索',
  'https://search.yahoo.co.jp/realtime/search?p=',
);

// Wikipedia jp
addSearchAlias(
  'wi',
  'Wikipedia',
  'https://ja.wikipedia.org/w/index.php?search=',
);

// MDN
addSearchAlias('mdn', 'MDN', 'https://developer.mozilla.org/ja/search?q=');

// npm
addSearchAlias(
  'npm',
  'npm',
  'https://www.npmjs.com/search?q=',
  's',
  'https://api.npms.io/v2/search/suggestions?size=20&q=',
  (response) =>
    JSON.parse(response.text).map((s) => {
      let flags = '';
      let desc = '';
      let stars = '';
      let score = '';
      if (s.package.description) {
        desc = escapeForAlias(s.package.description);
      }
      if (s.score && s.score.final) {
        score = Math.round(Number(s.score.final) * 5);
        stars = '⭐'.repeat(score) + '☆'.repeat(5 - score);
      }
      if (s.flags) {
        Object.keys(s.flags).forEach((f) => {
          flags += `[<span style='color:#ff4d00'>⚑</span> ${escapeForAlias(
            f,
          )}] `;
        });
      }
      return createSuggestionItem(
        `
      <div>
        <style>.title>em { font-weight: bold; }</style>
        <div class="title">${s.highlight}</div>
        <div>
          <span style="font-size:1.5em;line-height:1em">${stars}</span>
          <span>${flags}</span>
        </div>
        <div>${desc}</div>
      </div>
    `,
        { url: s.package.links.npm },
      );
    }),
);

// Amazon jp
addSearchAlias(
  'am',
  'Amazon',
  'https://www.amazon.co.jp/s?k=',
  's',
  'https://completion.amazon.co.jp/search/complete?method=completion&search-alias=aps&mkt=6&q=',
  (response) => JSON.parse(response.text)[1],
);

// Amazon jp Kindle
addSearchAlias(
  'k',
  'Amazon Kindle',
  'https://www.amazon.co.jp/s?i=digital-text&k=',
  's',
  'https://completion.amazon.co.jp/search/complete?method=completion&search-alias=aps&mkt=6&q=',
  (response) => JSON.parse(response.text)[1],
);

// alc
addSearchAlias('alc', 'alc', 'https://eow.alc.co.jp/search?q=');
mapkey('oa', '#8Open Search with alias a', function () {
  Front.openOmnibar({ type: 'SearchEngine', extra: 'alc' });
});

// mercari
addSearchAlias('me', 'mercari', 'https://www.mercari.com/jp/search/?keyword=');

// ---- Mapkeys ----//
const copyTitleAndUrl = (format) => {
  const text = format
    .replace('%URL%', location.href)
    .replace('%TITLE%', document.title);
  Clipboard.write(text);
};
const copyHtmlLink = () => {
  const clipNode = document.createElement('a');
  const range = document.createRange();
  const sel = window.getSelection();
  clipNode.setAttribute('href', location.href);
  clipNode.innerText = document.title;
  document.body.appendChild(clipNode);
  range.selectNode(clipNode);
  sel.removeAllRanges();
  sel.addRange(range);
  document.execCommand('copy', false, null);
  document.body.removeChild(clipNode);
  Front.showBanner('Ritch Copied: ' + document.title);
};

mapkey('cm', '#7Copy title and link to markdown', () => {
  copyTitleAndUrl('[%TITLE%](%URL%)');
});
mapkey('cM', '#7Copy all tabs to markdown', () => {
  RUNTIME('getTabs', { queryInfo: { currentWindow: true } }, (response) => {
    const tabUrls = response.tabs
      .map((tab) => `- [${tab.title}](${tab.url})`)
      .join('\n');

    Clipboard.write(tabUrls);
  });
});
mapkey('ct', '#7Copy title and link to textile', () => {
  copyTitleAndUrl('"%TITLE%":%URL%');
});
mapkey('ch', '#7Copy title and link to human readable', () => {
  copyTitleAndUrl('%TITLE% / %URL%');
});
mapkey('cb', '#7Copy title and link to scrapbox', () => {
  copyTitleAndUrl('[%TITLE% %URL%]');
});
mapkey('ca', '#7Copy title and link to href', () => {
  copyTitleAndUrl('<a href="%URL%">%TITLE%</a>');
});
mapkey('cp', '#7Copy title and link to plantuml', () => {
  copyTitleAndUrl('[[%URL% %TITLE%]]');
});
mapkey('cr', '#7Copy rich text link', () => {
  copyHtmlLink();
});
mapkey('co', '#7Copy title and link to org', () => {
  copyTitleAndUrl('[[%URL%][%TITLE%]]');
});
mapkey('cO', '#7Copy all tabs to org', () => {
  RUNTIME('getTabs', { queryInfo: { currentWindow: true } }, (response) => {
    const tabUrls = response.tabs
      .map((tab) => `- [[${tab.url}][${tab.title}]]`)
      .join('\n');

    Clipboard.write(tabUrls);
  });
});
//  8: Omnibar
//  9: Visual Mode
// 10: vim-like marks
// 11: Settings
// 12: Chrome URLs
mapkey('gS', '#12Open Chrome settings', () =>
  tabOpenLink('chrome://settings/'),
);
// 13: Proxy
// 14: Misc
const createAiQuery = () => {
  const title = document.title;
  const url = window.location.href;
  const query = `${title} ${url} を日本語で要約してください。
要約には以下の要素を含めてください：
- **主要なポイント** - ページの重要な情報や主張をいくつか箇条書きで示してください。
- **具体例やケーススタディ** - 記載されている例やケーススタディがあれば、それも箇条書きに含めてください。
- **結論や推奨事項** - ページで示されている結論や推奨事項を簡潔にまとめてください。
- **重要なデータや統計** - ページ中の特筆すべきデータや統計があればその要点を示してください。
`;
  return encodeURIComponent(query);
};

mapkey('=f', '#14summarize With Felo AI', () => {
  tabOpenBackground(`https://felo.ai/search?q=${createAiQuery()}`);
});

mapkey('=o', '#14summarize With OpenAI ChatGPT', () => {
  tabOpenBackground(`https://chatgpt.com/?q=${createAiQuery()}`);
});

mapkey('=c', '#14summarize With Anthropic Claude', () => {
  tabOpenBackground(`https://claude.ai/new?q=${createAiQuery()}`);
});

mapkey('=b', '#14summarize With Bing', () => {
  tabOpenBackground(
    `https://www.bing.com/search?showconv=1&sendquery=1&q=${createAiQuery()}`,
  );
});

mapkey('=p', '#14summarize With PerplexityAI', () => {
  tabOpenBackground(`https://www.perplexity.ai/?q=${createAiQuery()}`);
});

unmap(';t');
mapkey(';t', '#14google translate', () => {
  const selection = window.getSelection().toString();
  if (selection === '') {
    // 文字列選択してない場合はページ自体を翻訳にかける
    tabOpenLink(
      `https://translate.google.com/translate?js=n&sl=auto&tl=ja&u=${window.location.href}`,
    );
  } else {
    // 選択している場合はそれを翻訳する
    tabOpenLink(
      `https://translate.google.com/?sl=auto&tl=ja&text=${encodeURI(
        selection,
      )}`,
    );
  }
});

mapkey(';b', '#14hatena bookmark', () => {
  const { location } = window;
  let url = location.href;
  if (location.href.startsWith('https://app.getpocket.com/read/')) {
    url = decodeURIComponent(
      document
        .querySelector('header a')
        .getAttribute('href')
        .replace('https://getpocket.com/redirect?url=', ''),
    );
  }
  if (url.startsWith('http:')) {
    tabOpenBackground(
      `http://b.hatena.ne.jp/entry/${url.replace('http://', '')}`,
    );
    return;
  }
  if (url.startsWith('https:')) {
    tabOpenBackground(
      `http://b.hatena.ne.jp/entry/s/${url.replace('https://', '')}`,
    );
    return;
  }
  throw new Error('はてなブックマークに対応していないページ');
});

mapkey(';G', '#14魚拓', () => {
  tabOpenLink(`https://megalodon.jp/?url=${location.href}`);
});

// org
const escapeChars = ['(', ')', "'"];
const escapeForOrg = (text) => {
  let _text = text;
  escapeChars.forEach((char) => {
    _text = _text.replaceAll(char, escape(char));
  });
  return _text;
};
const getUrl = (path, query) => {
  const queryString = Object.entries(query)
    .map(
      ([key, value]) =>
        `${encodeURIComponent(key)}=${escapeForOrg(encodeURIComponent(value))}`,
    )
    .join('&');
  return [path, queryString].join('?');
};
const orgCapture = (template) => {
  const url = getUrl('org-protocol://capture', {
    template,
    url: window.location.href,
    title: document.title.replace(/\|/g, '-'),
    body: window.getSelection(),
  });
  console.log(`orgCapture: ${url}`);
  window.location.href = url;
};
const orgRoamCapture = (template, option = {}) => {
  const url = getUrl('org-protocol://roam-ref', {
    template,
    ref: window.location.href,
    title: document.title,
    body: window.getSelection(),
    releaseDate: '',
    creator: '',
    ...option,
  });
  console.log(`orgRoamCapture: ${url}`);
  window.location.href = url;
};
mapkey(',m', '#14org-capture memo', () => {
  orgCapture('M');
});
mapkey(',t', '#14org-capture todo', () => {
  orgCapture('T');
});
mapkey(',l', '#14org-capture read it later', () => {
  orgCapture('L');
});
mapkey(',r', '#14org-roam-capture ref', () => {
  orgRoamCapture('r');
});
mapkey(',z', '#14org-roam-capture Resonance Calendar)', () => {
  switch (window.location.hostname) {
    case 'booklog.jp': {
      let [, releaseDate] = document
        .querySelector('.item-area-info')
        .innerText.match(/\/ (.*?発売)/);
      orgRoamCapture('z', {
        title: document.querySelector('.item-area-info-title').innerText,
        ref:
          location.origin +
          document
            .querySelector('.item-area-info-title a')
            .getAttribute('href'),
        type: 'Book',
        creator: document.querySelector('.author-link').innerText,
        releaseDate,
      });
      return;
    }
    case 'www.amazon.co.jp': {
      const url = `https://www.amazon.co.jp/dp/${
        document.querySelectorAll("[name='ASIN'], [name='ASIN.0']")[0].value
      }`;
      orgRoamCapture('z', {
        title: document.querySelector('#productTitle').innerText,
        ref: url,
        type: 'Book',
        creator: document.querySelector('.author a').innerText,
        releaseDate: document.querySelector(
          '#rpi-attribute-book_details-publication_date > div.a-section.a-spacing-none.a-text-center.rpi-attribute-value > span',
        ).innerText,
      });
      return;
    }
    case 'www.youtube.com': {
      orgRoamCapture('z', { type: 'Video' });
      return;
    }
    default:
      orgRoamCapture('z', { type: 'Web' });
  }
});

mapkey('=q', '#14Delete query', () => {
  location.href = location.href.replace(/\?.*/, '');
});
mapkey('=h', '#14Delete hash', () => {
  location.href = location.href.replace(/\#.*/, '');
});
// 15: Insert Mode

// ---- qmarks ----
const qmarksMapKey = (prefix, urls, newTab) => {
  const openLink = (link, newTab) => () => {
    RUNTIME('openLink', {
      tab: { tabbed: newTab },
      url: link,
    });
  };
  for (const key in urls) {
    mapkey(prefix + key, `qmark: ${urls[key]}`, openLink(urls[key], newTab));
  }
};
const qmarksUrls = {
  m: 'https://mail.google.com/mail/u/0/',
  h: 'http://b.hatena.ne.jp/hush_in/hotentry',
  i: 'https://www.instapaper.com/u',
  x: 'https://x.com/',
  b: 'https://bsky.app/',
  y: 'https://www.youtube.com/',
};
unmap('gn');
qmarksMapKey('gn', qmarksUrls, true);
qmarksMapKey('gO', qmarksUrls, false);

// --- Site-specific mappings ---
const clickElm = (selector) => () => document.querySelector(selector).click();

if (window.location.href.startsWith('https://docs.google.com/presentation/')) {
  const target = document.getElementById('docs-chrome');
  const focusDoc = () => {
    // ページ送り後に何故かフォーカスが about:blank のページに飛ぶので、フォーカスを当て直す
    setTimeout(() => {
      target.focus();
    }, 10);
  };
  focusDoc();
  mapkey(']', 'next page', () => {
    sendArrowKey('ArrowRight');
    focusDoc();
  });
  mapkey('[', 'prev page', () => {
    sendArrowKey('ArrowLeft');
    focusDoc();
  });
}

if (window.location.href.startsWith('https://drive.google.com/file/d/')) {
  const goToPage = (pageNumber) => {
    const input = document.querySelector('input');
    console.log(`ページ ${pageNumber} にジャンプします...`, input);
    if (!input) {
      console.error('ページ番号入力欄が見つかりません');
      return;
    }
    const enterEvent = new KeyboardEvent('keydown', {
      key: 'Enter',
      code: 'Enter',
      keyCode: 13,
      bubbles: true,
    });

    // ページ要素をクリックしていないと1回ではページ送りできないので、2回実行する
    for (let i = 0; i < 2; i++) {
      input.select();
      input.value = pageNumber.toString();
      input.dispatchEvent(new Event('input', { bubbles: true }));
      input.dispatchEvent(enterEvent);
    }

    console.log(`ページ ${pageNumber} にジャンプしました`);
  };

  // 現在のページ数を取得
  const getCurrentPage = () => {
    const input = document.querySelector('input');
    return input ? parseInt(input.value) : null;
  };

  // 総ページ数を取得
  const getTotalPages = () => {
    const input = document.querySelector('input');
    const parentDiv = input.parentElement;
    const grandParentDiv = parentDiv?.parentElement;
    // スラッシュ（/）の次の兄弟要素を探す
    const children = Array.from(grandParentDiv.children);
    const slashIndex = children.findIndex(
      (child) => child.textContent.trim() === '/',
    );
    const pageCountDiv = children[slashIndex + 1];
    return pageCountDiv ? parseInt(pageCountDiv.textContent) : null;
  };

  // 次のページへ
  const nextPage = () => {
    const current = getCurrentPage();
    const total = getTotalPages();

    if (current && total && current < total) {
      return goToPage(current + 1);
    }

    console.log('これが最後のページです');
  };

  // 前のページへ
  const prevPage = () => {
    const current = getCurrentPage();

    if (current && current > 1) {
      return goToPage(current - 1);
    }

    console.log('これが最初のページです');
  };
  mapkey(']', 'next page', nextPage);
  mapkey('[', 'prev page', prevPage);
}

if (window.location.hostname === 'booklog.jp') {
  mapkey(']', 'next page', clickElm('#modal-review-next'));
  mapkey('[', 'prev page', clickElm('#modal-review-prev'));
  mapkey('d', '読み終わった', clickElm('#status3'));
  mapkey('R', 'Read by Kindle', () =>
    RUNTIME('openLink', {
      tab: { tabbed: true },
      url: `https://read.amazon.co.jp/?asin=${document
        .querySelector('.item-area-info-title a')
        .getAttribute('href')
        .replace(/.*\//, '')}`,
    }),
  );
}

if (window.location.hostname === 'www.amazon.co.jp') {
  mapkey('=s', '#14URLを短縮', () => {
    location.href = `https://www.amazon.co.jp/dp/${
      document.querySelectorAll("[name='ASIN'], [name='ASIN.0']")[0].value
    }`;
  });
}

if (
  /^https:\/\/b.hatena.ne.jp\/.*\/hotentry\?date/.test(window.location.href)
) {
  const moveDate = (diff) => () => {
    const url = new URL(window.location.href);
    const dateTxt = url.searchParams.get('date');
    const [_, yyyy, mm, dd] = dateTxt.match(/(....)(..)(..)/);
    const date = new Date(
      parseInt(yyyy, 10),
      parseInt(mm, 10) - 1,
      parseInt(dd, 10) + diff,
    );
    url.searchParams.set('date', formatDate(date, 'YYYYMMDD'));
    location.href = url.href;
  };
  mapkey(']', 'next date', moveDate(1));
  mapkey('[', 'prev date', moveDate(-1));
}

// Video Speed Controller keys
const videoSpeedKeys = ['d', 's', 'z', 'x', 'r', 'g', 'v'];

if (
  ['mail.google.com', 'x.com', 'feedly.com', 'www.figma.com/file'].some(
    (domain) => window.location.href.startsWith(`https://${domain}`),
  )
) {
  unmapKeys([
    'j',
    'k',
    'o',
    'E',
    'R',
    'd',
    'u',
    'T',
    'f',
    'F',
    'C',
    'x',
    'S',
    'H',
    'L',
    '[',
    ']',
    // 'cm',
    // 'co',
    // 'ch',
    // ',t',
    // ',m',
  ]);
}

if (
  ['www.amazon.co.jp/gp/video', 'www.netflix.com/watch'].some((domain) =>
    window.location.href.startsWith(`https://${domain}`),
  )
) {
  // Video Speed Controller
  unmapKeys(videoSpeedKeys);
}

if (
  ['www.youtube.com/watch'].some((domain) =>
    window.location.href.startsWith(`https://${domain}`),
  )
) {
  map('C', 'c');
  // Video Control, Video Speed Controller
  unmapKeys([...videoSpeedKeys, 'h', 'j', 'k', 'l', 'f', 't']);
}

// click `Save` button to make above settings to take effect.

// refs.
// * https://github.com/b0o/surfingkeys-conf
// * https://github.com/ncaq/surfingkeys-ncaq
