import Block, { Position } from "./Block";
import { Grid } from "./Game";

interface BoardProps {
  grid: Grid;
  numOfColumns: number;
  onLaneClick: (lane: number) => void;
  hints?: {
    column: number;
    block: number;
    score: number;
    combos: number;
    max: number;
  }[];
}

function Board({ grid, numOfColumns, onLaneClick, hints = [] }: BoardProps) {
  const numOfRows = grid.length / numOfColumns;
  return (
    <div className="board" style={{ position: "relative" }}>
      <div
        className="blocks"
        style={{
          display: "grid",
          position: "relative",
          gridTemplateColumns: `repeat(${numOfColumns}, 70px)`,
          gridTemplateRows: `repeat(${numOfRows}, 70px)`,
        }}
      >
        {Array.from({ length: numOfColumns }).map((_, i) => (
          <div
            className="lane"
            style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
            onClick={() => onLaneClick(i + 1)}
            key={i}
          />
        ))}

        {grid.map((num, i) => {
          if (num === "-") return null;
          const pos: Position = [
            Math.floor(i / numOfColumns),
            i % numOfColumns,
          ];
          return <Block value={num} position={pos} key={i} />;
        })}

        {hints.map((hint) => (
          <div
            key={`hint-${hint.column}`}
            className="hint"
            style={{
              gridColumn: hint.column,
              gridRow: 1,
              alignSelf: "start",
              justifySelf: "center",
              zIndex: 2,
              background: "rgba(51, 41, 40, 0.8)",
              padding: "4px 6px",
              borderRadius: "6px",
              fontSize: "0.75rem",
              textAlign: "center",
              pointerEvents: "none",
            }}
          >
            +{hint.score} pts
            <br />x{hint.combos} combo
            <br />
            Max {hint.max}
          </div>
        ))}
      </div>
    </div>
  );
}

export default Board;
