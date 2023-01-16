use macroquad::prelude::*;

const WINDOW_SIZE: f32 = 600.0;

const BOARD_WHITE: Color = color_u8!(237, 238, 209, 255);
const BOARD_BLACK: Color = color_u8!(119, 153, 82, 255);
const BOARD_HIGHLIGHT: Color = color_u8!(255, 255, 0, 255.0 * 0.5);
const BOARD_HINT: Color = color_u8!(0, 0, 0, 255.0 * 0.1);
const BOARD_TILE_SIZE: f32 = WINDOW_SIZE / 8.0;
const BOARD_HINT_SMALL: f32 = BOARD_TILE_SIZE / 5.8;
const BOARD_HINT_BIG: f32 = BOARD_TILE_SIZE / 2.0;
const BOARD_HINT_BIG_THICK: f32 = BOARD_TILE_SIZE / 11.6;

const TEXT_COLOR: Color = color_u8!(38, 36, 33, 255);

const CHECK_DEPTH: usize = 1;

fn win_conf() -> Conf {
  Conf {
    window_title: "Chess-rs".to_string(),
    window_width: WINDOW_SIZE as i32,
    window_height: WINDOW_SIZE as i32,
    window_resizable: false,
    ..Default::default()
  }
}

fn validate_pos(x: u8, y: u8) -> Option<()> {
  if x < b'a' || x > b'h' {
    return None;
  }
  if y < b'1' || y > b'8' {
    return None;
  }

  Some(())
}

fn add_to_pos(initial: &str, x: i8, y: i8) -> Option<String> {
  let initial = initial.as_bytes();

  if initial[0] as i16 + x as i16 > i8::MAX as i16
    || initial[1] as i16 + y as i16 > i8::MAX as i16
    || initial[0] as i16 + (x as i16) < i8::MIN as i16
    || initial[1] as i16 + (y as i16) < i8::MIN as i16
  {
    return None;
  }

  let x = (initial[0] as i8 + x) as u8;
  let y = (initial[1] as i8 + y) as u8;

  validate_pos(x, y)?;

  let mut pos = String::with_capacity(3);
  pos.push(x as char);
  pos.push(y as char);

  Some(pos)
}

#[derive(Clone)]
struct AvailableMoves {
  moves: Vec<String>,
  attacks: Vec<String>,

  // Special moves
  en_passants: Vec<String>,
  castles: Vec<((i8, i8), String, String)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PieceType {
  Pawn,
  Bishop,
  Knight,
  Rook,
  Queen,
  King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PieceColor {
  Black,
  White,
}

#[derive(Debug, Clone, Copy)]
struct Piece {
  ty: PieceType,
  color: PieceColor,
  preview_moves: bool,
  first_move: bool,
  en_passant: bool,
}

impl Piece {
  fn get_available_moves(&self, pos: &str, board: &Board, depth: usize) -> AvailableMoves {
    let mut available_moves =
      AvailableMoves { moves: vec![], attacks: vec![], en_passants: vec![], castles: vec![] };

    fn add_glide_move(
      piece: &Piece,
      board: &Board,
      initial: &str,
      available_moves: &mut AvailableMoves,
      add_to_moves: bool,
      add_to_attacks: bool,
      x: i8,
      y: i8,
      depth: usize,
    ) {
      let mov = add_to_pos(initial, x, y);

      if let Some(mov) = mov {
        let in_check_after = board.is_in_check_after(initial, &mov, piece.color, depth);

        if !in_check_after || (!in_check_after && board.is_in_check(piece.color, depth)) {
          let tile = board.get_tile_at(&mov.to_string()).unwrap();

          if let Tile::Empty = tile {
            if add_to_moves {
              available_moves.moves.push(mov);
            }
          } else if let Tile::Piece(Piece { color, .. }) = tile {
            if add_to_attacks && color != piece.color {
              available_moves.attacks.push(mov);
            }
          }
        }
      }
    }

    fn add_slide_move(
      piece: &Piece,
      board: &Board,
      initial: &str,
      available_moves: &mut AvailableMoves,
      add_to_moves: bool,
      add_to_attacks: bool,
      count: i8,
      x: i8,
      y: i8,
      depth: usize,
    ) {
      let mut added_moves = vec![];
      let mut added_attacks = vec![];

      for i in 1..=count {
        let mov = add_to_pos(initial, x * i, y * i);

        if let Some(mov) = mov {
          let in_check_after = board.is_in_check_after(initial, &mov, piece.color, depth);

          let tile = board.get_tile_at(&mov.to_string()).unwrap();

          if let Tile::Empty = tile {
            if !in_check_after || (!in_check_after && board.is_in_check(piece.color, depth)) {
              added_moves.push(mov);
            }
          } else if let Tile::Piece(Piece { color, .. }) = tile {
            if piece.ty != PieceType::Pawn
              && color != piece.color
              && (!in_check_after || (!in_check_after && board.is_in_check(piece.color, depth)))
            {
              added_attacks.push(mov);
            }
            break;
          }
        }
      }

      if add_to_moves {
        available_moves.moves.extend(added_moves);
      }

      if add_to_attacks {
        available_moves.attacks.extend(added_attacks);
      }
    }

    fn add_king_move(
      piece: &Piece,
      board: &Board,
      initial: &str,
      available_moves: &mut AvailableMoves,
      add_to_moves: bool,
      add_to_attacks: bool,
      x: i8,
      y: i8,
      depth: usize,
    ) {
      let mov = add_to_pos(initial, x, y);
      if let Some(mov) = mov {
        if !board.is_in_check_after(initial, &mov, piece.color, depth) {
          let tile = board.get_tile_at(&mov.to_string()).unwrap();
          if let Tile::Empty = tile {
            if add_to_moves {
              available_moves.moves.push(mov);
            }
          } else if let Tile::Piece(Piece { color, .. }) = tile {
            if add_to_attacks && color != piece.color {
              available_moves.attacks.push(mov);
            }
          }
        }
      }
    }

    match self.ty {
      PieceType::Pawn => {
        fn add_en_passant(
          piece: &Piece,
          board: &Board,
          initial: &str,
          available_moves: &mut AvailableMoves,
          x: i8,
          y: i8,
          depth: usize,
        ) {
          let to_be_en_passant = add_to_pos(initial, x, 0);

          if let Some(to_be_en_passant) = to_be_en_passant {
            let to_be_en_passant = board.get_tile_at(&to_be_en_passant.to_string()).unwrap();
            if let Tile::Piece(Piece { ty: PieceType::Pawn, color, en_passant: true, .. }) =
              to_be_en_passant
            {
              if color != piece.color {
                let mov = add_to_pos(initial, x, y);

                if let Some(mov) = mov {
                  let in_check_after = board.is_in_check_after(initial, &mov, piece.color, depth);

                  if !in_check_after || (!in_check_after && board.is_in_check(piece.color, depth)) {
                    available_moves.en_passants.push(mov);
                  }
                }
              }
            }
          }
        }

        let y = if self.color == PieceColor::Black { -1 } else { 1 };
        add_slide_move(
          self,
          board,
          pos,
          &mut available_moves,
          true,
          false,
          if self.first_move { 2 } else { 1 },
          0,
          y,
          depth,
        );
        add_glide_move(self, board, pos, &mut available_moves, false, true, -1, y, depth);
        add_glide_move(self, board, pos, &mut available_moves, false, true, 1, y, depth);

        add_en_passant(self, board, pos, &mut available_moves, -1, y, depth);
        add_en_passant(self, board, pos, &mut available_moves, 1, y, depth);
      },
      PieceType::Bishop => {
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, -1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, -1, depth);
      },
      PieceType::Knight => {
        add_glide_move(self, board, pos, &mut available_moves, true, true, -1, 2, depth);
        add_glide_move(self, board, pos, &mut available_moves, true, true, -2, 1, depth);

        add_glide_move(self, board, pos, &mut available_moves, true, true, 1, 2, depth);
        add_glide_move(self, board, pos, &mut available_moves, true, true, 2, 1, depth);

        add_glide_move(self, board, pos, &mut available_moves, true, true, -1, -2, depth);
        add_glide_move(self, board, pos, &mut available_moves, true, true, -2, -1, depth);

        add_glide_move(self, board, pos, &mut available_moves, true, true, 1, -2, depth);
        add_glide_move(self, board, pos, &mut available_moves, true, true, 2, -1, depth);
      },
      PieceType::Rook => {
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, 0, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, 0, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 0, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 0, -1, depth);
      },
      PieceType::Queen => {
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, -1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, -1, depth);

        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 1, 0, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, -1, 0, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 0, 1, depth);
        add_slide_move(self, board, pos, &mut available_moves, true, true, 99, 0, -1, depth);
      },
      PieceType::King => {
        fn add_castle(
          piece: &Piece,
          board: &Board,
          initial: &str,
          with: &str,
          available_moves: &mut AvailableMoves,
          count: i8,
          x: i8,
          y: i8,
          depth: usize,
        ) {
          if piece.first_move {
            for i in 1..=count {
              let mov = add_to_pos(initial, x * i, y * i);

              if let Some(mov) = mov {
                let tile = board.get_tile_at(&mov.to_string()).unwrap();
                if let Tile::Empty = tile {
                } else {
                  return;
                }
              }
            }

            let mov = add_to_pos(initial, x * count, y * count);

            if let Some(mov) = mov {
              if !board.is_in_check(piece.color, depth)
                && !board.is_in_check_after(initial, &mov, piece.color, depth)
              {
                let piece = board.get_tile_at(initial).unwrap();
                let with_piece = board.get_tile_at(with).unwrap();

                if let (
                  Tile::Piece(Piece { first_move: true, .. }),
                  Tile::Piece(Piece { first_move: true, .. }),
                ) = (piece, with_piece)
                {
                  available_moves.castles.push(((x, y), mov, with.to_string()));
                }
              }
            }
          }
        }

        add_king_move(self, board, pos, &mut available_moves, true, true, 1, 1, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, 1, 0, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, 1, -1, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, 0, -1, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, -1, -1, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, -1, 0, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, -1, 1, depth);
        add_king_move(self, board, pos, &mut available_moves, true, true, 0, 1, depth);

        if self.color == PieceColor::Black {
          add_castle(self, board, pos, "h8", &mut available_moves, 2, 1, 0, depth);
          add_castle(self, board, pos, "a8", &mut available_moves, 2, -1, 0, depth);
        } else {
          add_castle(self, board, pos, "h1", &mut available_moves, 2, 1, 0, depth);
          add_castle(self, board, pos, "a1", &mut available_moves, 2, -1, 0, depth);
        }
      },
    }

    available_moves
  }
}

#[derive(Debug, Clone, Copy)]
enum Tile {
  Empty,
  Piece(Piece),
}

#[derive(Clone)]
struct Board {
  state: [Tile; 64],
  moves: Vec<(String, Tile)>,
  curr_color: PieceColor,
  available_moves: Option<AvailableMoves>,
  draw: bool,
  winner: Option<PieceColor>,
}

impl Board {
  pub fn new(black: Vec<&str>, white: Vec<&str>) -> Option<Board> {
    let mut state = [Tile::Empty; 64];

    for pos in black {
      let (ty, i) = Board::parse_notation(pos)?;
      state[i] = Tile::Piece(Piece {
        ty,
        color: PieceColor::Black,
        preview_moves: false,
        first_move: true,
        en_passant: false,
      });
    }

    for pos in white {
      let (ty, i) = Board::parse_notation(pos)?;
      state[i] = Tile::Piece(Piece {
        ty,
        color: PieceColor::White,
        preview_moves: false,
        first_move: true,
        en_passant: false,
      });
    }

    Some(Board {
      state,
      moves: vec![],
      curr_color: PieceColor::White,
      available_moves: None,
      draw: false,
      winner: None,
    })
  }

  pub fn update(&mut self) {
    let mut clicked_on = None;

    for (i, piece) in self.state.iter().enumerate().filter_map(|(i, tile)| match tile {
      Tile::Piece(piece) if piece.color == self.curr_color => Some((i, piece)),
      _ => None,
    }) {
      let (mouse_x, mouse_y) = mouse_position();

      if piece.preview_moves && is_mouse_button_pressed(MouseButton::Left) {
        if let Some(available_moves) = &self.available_moves {
          if let Some(j) =
            available_moves.moves.iter().chain(available_moves.attacks.iter()).find_map(|mov| {
              let i = Board::parse_pos(mov).unwrap();

              let x = i / 8;
              let y = i % 8;

              Some(i).filter(|_| {
                Rect::new(
                  x as f32 * BOARD_TILE_SIZE,
                  y as f32 * BOARD_TILE_SIZE,
                  BOARD_TILE_SIZE,
                  BOARD_TILE_SIZE,
                )
                .contains(vec2(mouse_x, mouse_y))
                  && is_mouse_button_pressed(MouseButton::Left)
              })
            })
          {
            self.move_piece(piece.ty, i, j);
            // Turn off all the preview_moves
            clicked_on = Some(999);

            break;
          } else if let Some(j) = available_moves.en_passants.iter().find_map(|mov| {
            let i = Board::parse_pos(mov).unwrap();

            let x = i / 8;
            let y = i % 8;

            Some(i).filter(|_| {
              Rect::new(
                x as f32 * BOARD_TILE_SIZE,
                y as f32 * BOARD_TILE_SIZE,
                BOARD_TILE_SIZE,
                BOARD_TILE_SIZE,
              )
              .contains(vec2(mouse_x, mouse_y))
                && is_mouse_button_pressed(MouseButton::Left)
            })
          }) {
            self.move_piece(piece.ty, i, j);

            let dest_pos = Board::get_pos(j).unwrap();
            let below_dest = add_to_pos(&dest_pos, 0, -1).unwrap();
            self.state[Board::parse_pos(&below_dest).unwrap()] = Tile::Empty;

            // Turn off all the preview_moves
            clicked_on = Some(999);

            break;
          } else if let Some((dir, j, k, mov)) =
            available_moves.castles.iter().find_map(|(dir, mov, with)| {
              let i = Board::parse_pos(mov).unwrap();
              let j = Board::parse_pos(with).unwrap();

              let x = i / 8;
              let y = i % 8;

              Some((*dir, i, j, mov.to_string())).filter(|_| {
                Rect::new(
                  x as f32 * BOARD_TILE_SIZE,
                  y as f32 * BOARD_TILE_SIZE,
                  BOARD_TILE_SIZE,
                  BOARD_TILE_SIZE,
                )
                .contains(vec2(mouse_x, mouse_y))
                  && is_mouse_button_pressed(MouseButton::Left)
              })
            })
          {
            self.move_piece(piece.ty, i, j);

            let with_pos = add_to_pos(&mov, dir.0 * -1, dir.1 * -1).unwrap();
            let with_dest = Board::parse_pos(&with_pos).unwrap();
            self.state[with_dest] = self.state[k];
            self.state[k] = Tile::Empty;

            // Turn off all the preview_moves
            clicked_on = Some(999);

            break;
          }
        }
      }

      let x = i / 8;
      let y = i % 8;

      if Rect::new(
        x as f32 * BOARD_TILE_SIZE,
        y as f32 * BOARD_TILE_SIZE,
        BOARD_TILE_SIZE,
        BOARD_TILE_SIZE,
      )
      .contains(vec2(mouse_x, mouse_y))
        && is_mouse_button_pressed(MouseButton::Left)
      {
        clicked_on = Some(i);

        break;
      }
    }

    if let Some(i) = clicked_on {
      for (j, piece) in self.state.iter_mut().enumerate().filter_map(|(j, tile)| match tile {
        Tile::Empty => None,
        Tile::Piece(piece) => Some((j, piece)),
      }) {
        piece.preview_moves = i == j;
      }

      if let Some(pos) = Board::get_pos(i) {
        if let Tile::Piece(piece) = self.state[i] {
          self.available_moves = Some(piece.get_available_moves(&pos, self, 0));
        }
      }
    }
  }

  fn move_piece(&mut self, ty: PieceType, initial: usize, dest: usize) {
    self.curr_color =
      if self.curr_color == PieceColor::Black { PieceColor::White } else { PieceColor::Black };
    self.moves.push((Board::get_notation(ty, dest).unwrap(), self.state[dest]));

    self.state[dest] = self.state[initial];
    if let Tile::Piece(piece) = &mut self.state[dest] {
      piece.en_passant = piece.ty == PieceType::Pawn
        && piece.first_move
        && (dest as i64 - initial as i64).abs() == 2;
      piece.first_move = false;
    }
    self.state[initial] = Tile::Empty;

    self.check_for_draw();
    self.check_for_checkmate();
  }

  pub fn draw(&self, pieces_textures: &PiecesTextures) {
    for y in 0..8 {
      for x in 0..8 {
        draw_rectangle(
          x as f32 * BOARD_TILE_SIZE,
          y as f32 * BOARD_TILE_SIZE,
          BOARD_TILE_SIZE,
          BOARD_TILE_SIZE,
          if ((y + 1) % 2 == 0 && (x + 1) % 2 != 0) || ((y + 1) % 2 != 0 && (x + 1) % 2 == 0) {
            BOARD_BLACK
          } else {
            BOARD_WHITE
          },
        );
      }
    }

    for (i, piece) in self.state.iter().enumerate().filter_map(|(i, tile)| match tile {
      Tile::Piece(piece) if piece.preview_moves => Some((i, piece)),
      _ => None,
    }) {
      if piece.preview_moves && self.available_moves.is_some() {
        self.draw_moves(&Board::get_pos(i).unwrap());
      }
    }

    for (i, piece) in self.state.iter().enumerate().filter_map(|(i, tile)| match tile {
      Tile::Empty => None,
      Tile::Piece(piece) => Some((i, piece)),
    }) {
      let x = i / 8;
      let y = i % 8;

      let texture = if piece.color == PieceColor::Black {
        match piece.ty {
          PieceType::Pawn => pieces_textures.black_pawn,
          PieceType::Bishop => pieces_textures.black_bishop,
          PieceType::Knight => pieces_textures.black_knight,
          PieceType::Rook => pieces_textures.black_rook,
          PieceType::Queen => pieces_textures.black_queen,
          PieceType::King => pieces_textures.black_king,
        }
      } else {
        match piece.ty {
          PieceType::Pawn => pieces_textures.white_pawn,
          PieceType::Bishop => pieces_textures.white_bishop,
          PieceType::Knight => pieces_textures.white_knight,
          PieceType::Rook => pieces_textures.white_rook,
          PieceType::Queen => pieces_textures.white_queen,
          PieceType::King => pieces_textures.white_king,
        }
      };

      draw_texture_ex(
        texture,
        x as f32 * BOARD_TILE_SIZE,
        y as f32 * BOARD_TILE_SIZE,
        WHITE,
        DrawTextureParams {
          dest_size: Some(vec2(BOARD_TILE_SIZE, BOARD_TILE_SIZE)),
          ..Default::default()
        },
      );
    }
  }

  fn draw_moves(&self, pos: &str) {
    let i = Board::parse_pos(pos).unwrap();
    let x = i / 8;
    let y = i % 8;

    draw_rectangle(
      x as f32 * BOARD_TILE_SIZE,
      y as f32 * BOARD_TILE_SIZE,
      BOARD_TILE_SIZE,
      BOARD_TILE_SIZE,
      BOARD_HIGHLIGHT,
    );

    if let Some(available_moves) = &self.available_moves {
      for mov in
        available_moves.moves.iter().chain(available_moves.castles.iter().map(|(_, mov, _)| mov))
      {
        let i = Board::parse_pos(mov).unwrap();
        let x = i / 8;
        let y = i % 8;

        draw_circle(
          x as f32 * BOARD_TILE_SIZE + BOARD_TILE_SIZE / 2.0,
          y as f32 * BOARD_TILE_SIZE + BOARD_TILE_SIZE / 2.0,
          BOARD_HINT_SMALL,
          BOARD_HINT,
        );
      }

      for mov in available_moves.attacks.iter().chain(available_moves.en_passants.iter()) {
        let i = Board::parse_pos(mov).unwrap();
        let x = i / 8;
        let y = i % 8;

        draw_circle_lines(
          x as f32 * BOARD_TILE_SIZE + BOARD_TILE_SIZE / 2.0,
          y as f32 * BOARD_TILE_SIZE + BOARD_TILE_SIZE / 2.0,
          BOARD_HINT_BIG,
          BOARD_HINT_BIG_THICK,
          BOARD_HINT,
        );
      }
    }
  }

  fn get_tile_at(&self, pos: &str) -> Option<Tile> {
    let pos = Board::parse_pos(pos)?;
    Some(self.state[pos])
  }

  fn is_pos_check(&self, pos: &str, color: PieceColor, depth: usize) -> bool {
    if depth > CHECK_DEPTH {
      return false;
    }

    for (i, piece) in self.state.iter().enumerate().filter_map(|(i, tile)| match tile {
      Tile::Piece(piece) if piece.color != color => Some((i, piece)),
      _ => None,
    }) {
      let piece_pos = Board::get_pos(i).unwrap();
      if piece.get_available_moves(&piece_pos, self, depth + 1).attacks.contains(&pos.to_string()) {
        return true;
      }
    }

    false
  }

  fn is_in_check(&self, color: PieceColor, depth: usize) -> bool {
    if let Some(i) = self.state.iter().enumerate().find_map(|(i, tile)| match tile {
      Tile::Piece(piece) if piece.color == color && piece.ty == PieceType::King => Some(i),
      _ => None,
    }) {
      let pos = Board::get_pos(i).unwrap();
      if self.is_pos_check(&pos, color, depth) {
        return true;
      }
    }

    false
  }

  fn is_in_check_after(&self, initial: &str, mov: &str, color: PieceColor, depth: usize) -> bool {
    let mut clone = self.clone();

    let initial = Board::parse_pos(initial).unwrap();
    let mov = Board::parse_pos(mov).unwrap();

    let before_mov = clone.state[mov];
    clone.state[mov] = clone.state[initial];
    clone.state[initial] = Tile::Empty;

    let in_check = clone.is_in_check(color, depth);

    clone.state[initial] = clone.state[mov];
    clone.state[mov] = before_mov;

    in_check
  }

  fn is_stalemate(&self, color: PieceColor) -> bool {
    self
      .state
      .iter()
      .enumerate()
      .filter_map(|(i, tile)| match tile {
        Tile::Piece(piece) if piece.color == color => Some((i, piece)),
        _ => None,
      })
      .all(|(i, piece)| {
        let pos = Board::get_pos(i).unwrap();
        let available_moves = piece.get_available_moves(&pos, self, 0);
        available_moves.moves.is_empty()
          && available_moves.attacks.is_empty()
          && available_moves.en_passants.is_empty()
      })
  }

  fn is_checkmate(&self, color: PieceColor) -> bool {
    self.is_in_check(color, 0) && self.is_stalemate(color)
  }

  fn check_for_draw(&mut self) {
    self.draw = self.is_stalemate(if self.curr_color == PieceColor::Black {
      PieceColor::White
    } else {
      PieceColor::Black
    });
  }

  fn check_for_checkmate(&mut self) {
    if self.is_checkmate(self.curr_color) {
      self.winner = Some(if self.curr_color == PieceColor::Black {
        PieceColor::White
      } else {
        PieceColor::Black
      });
    }
  }

  fn get_pos(i: usize) -> Option<String> {
    let x = ((i / 8) as u8 + b'a') as char;
    let y = ((8 - i % 8) as u8 + b'0') as char;
    validate_pos(x as u8, y as u8)?;

    let mut pos = String::with_capacity(3);
    pos.push(x);
    pos.push(y);

    Some(pos)
  }

  fn get_notation(ty: PieceType, i: usize) -> Option<String> {
    let piece = match ty {
      PieceType::Pawn => 'P',
      PieceType::Bishop => 'B',
      PieceType::Knight => 'N',
      PieceType::Rook => 'R',
      PieceType::Queen => 'Q',
      PieceType::King => 'K',
    };
    let dest = Board::get_pos(i)?;

    let mut notation = String::with_capacity(3);
    notation.push(piece);
    notation.push_str(&dest);

    Some(notation)
  }

  fn parse_pos(pos: &str) -> Option<usize> {
    let pos = pos.as_bytes();
    validate_pos(pos[0], pos[1])?;
    let dest = (pos[0] - b'a') as usize * 8 + (9 - (pos[1] - b'0')) as usize - 1;
    Some(dest)
  }

  fn parse_notation(notation: &str) -> Option<(PieceType, usize)> {
    let dest = Board::parse_pos(&notation[1..3])?;
    let notation = notation.as_bytes();
    let piece = match notation[0] {
      b'P' => PieceType::Pawn,
      b'B' => PieceType::Bishop,
      b'N' => PieceType::Knight,
      b'R' => PieceType::Rook,
      b'Q' => PieceType::Queen,
      b'K' => PieceType::King,
      _ => panic!("Invalid move notation!"),
    };

    Some((piece, dest))
  }
}

struct PiecesTextures {
  black_pawn: Texture2D,
  black_bishop: Texture2D,
  black_knight: Texture2D,
  black_rook: Texture2D,
  black_queen: Texture2D,
  black_king: Texture2D,

  white_pawn: Texture2D,
  white_bishop: Texture2D,
  white_knight: Texture2D,
  white_rook: Texture2D,
  white_queen: Texture2D,
  white_king: Texture2D,
}

impl PiecesTextures {
  pub async fn new() -> PiecesTextures {
    PiecesTextures {
      black_pawn: load_texture("assets/bp.png").await.expect("Cannot find textures for pieces!"),
      black_bishop: load_texture("assets/bb.png").await.expect("Cannot find textures for pieces!"),
      black_knight: load_texture("assets/bn.png").await.expect("Cannot find textures for pieces!"),
      black_rook: load_texture("assets/br.png").await.expect("Cannot find textures for pieces!"),
      black_queen: load_texture("assets/bq.png").await.expect("Cannot find textures for pieces!"),
      black_king: load_texture("assets/bk.png").await.expect("Cannot find textures for pieces!"),

      white_pawn: load_texture("assets/wp.png").await.expect("Cannot find textures for pieces!"),
      white_bishop: load_texture("assets/wb.png").await.expect("Cannot find textures for pieces!"),
      white_knight: load_texture("assets/wn.png").await.expect("Cannot find textures for pieces!"),
      white_rook: load_texture("assets/wr.png").await.expect("Cannot find textures for pieces!"),
      white_queen: load_texture("assets/wq.png").await.expect("Cannot find textures for pieces!"),
      white_king: load_texture("assets/wk.png").await.expect("Cannot find textures for pieces!"),
    }
  }
}

#[macroquad::main(win_conf)]
async fn main() {
  let font = load_ttf_font("assets/FreeSansBold.ttf").await.expect("Cannot find font!");

  #[rustfmt::skip]
  let mut board = Board::new(
    vec![
      "Ra8", "Nb8", "Bc8", "Qd8", "Ke8", "Bf8", "Ng8", "Rh8",
      "Pa7", "Pb7", "Pc7", "Pd7", "Pe7", "Pf7", "Pg7", "Ph7",
    ],
    vec![
      "Pa2", "Pb2", "Pc2", "Pd2", "Pe2", "Pf2", "Pg2", "Ph2",
      "Ra1", "Nb1", "Bc1", "Qd1", "Ke1", "Bf1", "Ng1", "Rh1",
    ]
  ).unwrap();

  let pieces_textures = PiecesTextures::new().await;

  loop {
    clear_background(BOARD_WHITE);

    board.update();
    board.draw(&pieces_textures);

    let winner = if board.winner == Some(PieceColor::Black) {
      Some("Black Wins!")
    } else if board.winner == Some(PieceColor::White) {
      Some("White Wins!")
    } else if board.draw {
      Some("Draw!")
    } else {
      None
    };

    if let Some(text) = winner {
      let text_measure = measure_text(text, Some(font), 64, 1.0);
      draw_text_ex(
        text,
        screen_width() / 2.0 - text_measure.width / 2.0,
        screen_height() / 2.0 - text_measure.height / 2.0 + text_measure.offset_y / 2.0,
        TextParams { color: TEXT_COLOR, font_size: 64, font, ..Default::default() },
      );
    }

    next_frame().await;
  }
}
