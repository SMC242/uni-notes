// From https://beingpax.medium.com/how-to-create-a-visual-progress-bar-in-obsidian-3016416dad19

// Create wrapper
const containerEl = createDiv();
Object.assign(containerEl.style, {
  display: "flex",
  "flex-direction": "column",
  "align-items": "center",
  "justify-content": "center",
});
const heading = containerEl.createEl("h2");
Object.assign(heading, { textContent: "Progress" });

// Calculate progress percentage
const { start, target, progress } = dv.page(input.file);
const isDescendingProgress = progress > target;
let max, value, percent;
if (isDescendingProgress && start) {
  max = (target - start) * -1;
  value = start - progress;
}
if (!isDescendingProgress && start) {
  max = target - start;
  value = progress - start;
}
if (!isDescendingProgress && !start) {
  max = target;
  value = progress;
}
percent = Math.round((value / max) * 100);

// Create progress bar and text
const progressBar = containerEl.createEl("progress");
Object.assign(progressBar, { max, value });
const progressText = containerEl.createEl("div");
Object.assign(progressText, { textContent: `${percent}% completed` });

// Output to dataview
dv.paragraph(containerEl);
