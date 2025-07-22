export function numberToColor(num: number): string {
  if (num <= 0 || (num & (num - 1)) !== 0) {
    return "black";
  }

  const power = Math.log2(num);
  const hue = (power * 40) % 360;
  const saturation = 70;
  const lightness = 50;

  return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

export function delay(milliseconds: number) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}
