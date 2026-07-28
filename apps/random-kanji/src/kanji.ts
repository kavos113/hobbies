const UNICODE_MAX = 0x9fff;
const UNICODE_MIN = 0x4e00;

export const setupKanjiRoller = (
  button: HTMLButtonElement,
  target: HTMLParagraphElement,
) => {
  const handler = () => {
    const unicode = Math.floor(
      Math.random() * (UNICODE_MAX - UNICODE_MIN) + UNICODE_MIN,
    );
    target.textContent = String.fromCodePoint(unicode);
  };

  button.addEventListener("click", handler);
};
