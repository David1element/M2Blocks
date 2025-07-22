import { useEffect, useState, useRef } from "react";
import PengineClient, { PrologTerm } from "./PengineClient";
import Board from "./Board";
import Block from "./Block";
import { delay } from "./util";

export type Grid = (number | "-")[];

type EffectInfoTerm =
  | NewBlockTerm
  | RemovedTerm
  | RangeChangedTerm
  | PrologTerm;

interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

interface RemovedTerm extends PrologTerm {
  functor: "removed";
  args: [number];
}

interface RangeChangedTerm extends PrologTerm {
  functor: "range_changed";
  args: [number, number];
}
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

function Game() {
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [nextBlock, setNextBlock] = useState<number | null>(null);
  const [isNextBlockVisible, setIsNextBlockVisible] = useState<boolean>(false);
  const [comboCount, setComboCount] = useState<number | null>(null);
  const [rangeMessage, setRangeMessage] = useState<string | null>(null);
  const maxBlockRef = useRef<number>(0);
  const [maxBlockMessage, setMaxBlockMessage] = useState<string | null>(null);
  const [RemoveBlockMessage, setRemoveBlockMessage] = useState<string | null>(
    null
  );
  const [hints, setHints] = useState<
    {
      column: number;
      block: number;
      score: number;
      combos: number;
      max: number;
    }[]
  >([]);

  useEffect(() => {
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      initGame();
    }
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const queryS =
      "init(Grid, NumOfColumns), randomBlock(Grid, Block1), randomBlock(Grid, Block2)";
    const response = await pengine!.query(queryS);
    setGrid(response["Grid"]);

    setShootBlock(response["Block1"]);
    setNextBlock(response["Block2"]);
    setNumOfColumns(response["NumOfColumns"]);
  }

  async function handleLaneClick(lane: number) {
    if (waiting) return;

    const gridS = JSON.stringify(grid).replace(/"/g, "");
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;

    setWaiting(true);
    setHints([]);

    const response = await pengine.query(queryS);
    if (response) {
      const newBlock = response["Block"];
      setShootBlock(nextBlock);
      setNextBlock(newBlock);

      animateEffect(response["Effects"]);
    } else {
      setWaiting(false);
    }
  }

  async function handleBoosterHint() {
    if (!grid || !numOfColumns || shootBlock == null) return;

    const gridS = JSON.stringify(grid).replace(/"/g, "");
    const hintPromises = Array.from({ length: numOfColumns }, async (_, i) => {
      const col = i + 1;
      const query = `hint(${shootBlock}, ${col}, ${gridS}, ${numOfColumns}, Hint)`;

      const response = await pengine.query(query);
      if (response && response["Hint"]) {
        const [column, block, score, combos, max] = response["Hint"].args;
        return { column, block, score, combos, max };
      } else {
        return {
          column: col,
          block: shootBlock,
          score: 0,
          combos: 0,
          max: 0,
        };
      }
    });

    const allHints = (await Promise.all(hintPromises)).filter(Boolean);
    setHints(allHints as typeof hints);
  }

  function handleNextBlockBooster() {
    setIsNextBlockVisible(true);
    setTimeout(() => {
      setIsNextBlockVisible(false);
    }, 3000);
  }

  function showRangeMessage(msg: string) {
    setRangeMessage(msg);
    setTimeout(() => setRangeMessage(null), 3500);
  }

  function showRemoveBlockMessage(msg: string) {
    setRemoveBlockMessage(msg);
    setTimeout(() => setRemoveBlockMessage(null), 3000);
  }

  function showMaxBlockMessage(msg: string) {
    setMaxBlockMessage(msg);
    setTimeout(() => setMaxBlockMessage(null), 2500);
  }

  async function animateEffect(effects: EffectTerm[], isFirstCall = true) {
    if (isFirstCall) {
      const totalNewBlocks = effects.reduce((acc, effect) => {
        const effectInfo = effect.args[1];
        const newCount = effectInfo.filter(
          (item) => item.functor === "newBlock"
        ).length;
        return acc + newCount;
      }, 0);

      if (totalNewBlocks > 1) {
        setComboCount(totalNewBlocks);
        setTimeout(() => setComboCount(null), 2000);
      }
    }

    if (effects.length === 0) {
      setWaiting(false);
      return;
    }

    const effect = effects[0];
    const [effectGrid, effectInfo] = effect.args;

    setGrid(effectGrid);

    effectInfo.forEach(async (effectInfoItem) => {
      const { functor, args } = effectInfoItem;

      if (functor === "newBlock") {
        const value = args[0];
        setScore((prev) => prev + value);

        if (value > maxBlockRef.current && value >= 512) {
          showMaxBlockMessage(`¡Nuevo bloque máximo: ${value}!`);
          maxBlockRef.current = value;
        }
      }

      if (functor === "removed") {
        const [min] = args;
        showRemoveBlockMessage(`¡Bloque Eliminado : ${min}!`);

        if (min === nextBlock) {
          const gridS = JSON.stringify(effectGrid).replace(/"/g, "");
          const queryS = `randomBlock(${gridS}, Block)`;
          const response = await pengine.query(queryS);

          if (response && response["Block"] !== undefined) {
            setShootBlock(response["Block"]);
          }
        }
      }

      if (functor === "range_changed") {
        const [low, high] = args;
        showRangeMessage(
          `¡Nuevo rango de disparo: Bloque Minimo ${low} - Bloque Maximo ${high}!`
        );
      }
    });

    const rest = effects.slice(1);

    if (rest.length === 0) {
      setWaiting(false);
      return;
    }

    await delay(50);
    animateEffect(rest, false);
  }

  if (grid === null || numOfColumns === null) return null;

  return (
    <div className="game" style={{ position: "relative" }}>
      <div className="header">
        <div className="score">{score}</div>
      </div>

      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
        hints={hints}
      />

      {(comboCount || rangeMessage || RemoveBlockMessage) && (
        <div className="game-overlay">
          {comboCount !== null && <div>🔥 Combo x {comboCount}</div>}
          {rangeMessage && <div>🎯 {rangeMessage}</div>}
          {RemoveBlockMessage && <div>ℹ️ {RemoveBlockMessage}</div>}
          {maxBlockMessage && <div>ℹ️ {maxBlockMessage}</div>}
        </div>
      )}

      <button onClick={handleBoosterHint}>Prediccion</button>
      <button onClick={handleNextBlockBooster}>Siguiente Bloque</button>

      <div className="footer">
        <div className="blockShoot">
          <Block value={shootBlock!} position={[0, 0]} />
        </div>
        {isNextBlockVisible && nextBlock != null && (
          <div className="next-block">
            <Block value={nextBlock} position={[0, 1]} />
          </div>
        )}
      </div>
    </div>
  );
}

export default Game;
