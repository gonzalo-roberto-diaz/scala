import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class GrayWhaleProj {

	public static void main(String arg[]) throws IOException {
		String chessPiece;
		int numPos = 0;
		// Get chess piece
		BufferedReader reader;
		reader = new BufferedReader(new InputStreamReader(System.in));
		System.out.println("What chess piece?");
		chessPiece = reader.readLine().toLowerCase();
		System.out.println(chessPiece);
		numPos = countNumbers(chessPiece);
		System.out.printf("Number of Combos: %d \n", numPos);
	}

	public static int countNumbers(String chessPiece) {

		int[][] num = new int[7][10];
		// initialize
		for (int from = 0; from < 10; from++)
			num[0][from] = 1;

		// use dynamic programming
		for (int numDigits = 1; numDigits < 7; numDigits++) {
			for (int from = 0; from < 10; from++) {

				int[] pos = telephonePad.getIndexOf(Character
						.forDigit(from, 10));

				ArrayList<Character> possibilities = getSuccessor(pos[0],
						pos[1], chessPiece);

				for (int index = 0; index < possibilities.size(); index++)
					num[numDigits][from] += num[numDigits - 1][Character.digit(
							possibilities.get(index), 10)];
			}
		}

		int numCombinations = 0;
		for (int from = 2; from < 10; from++)
			numCombinations += num[6][from];

		return numCombinations;
	}

	public static ArrayList<Character> getSuccessor(int i, int j,
			String chessPiece) {
		ArrayList<Character> possibilities;
		if (chessPiece.equals("queen")) {
			possibilities = telephonePad.getHorizontal(i, j);
			possibilities = merge(possibilities,
					(telephonePad.getVertical(i, j)));
			possibilities = merge(possibilities,
					(telephonePad.getDiagonal(i, j)));
		} else if (chessPiece.equals("knight"))
			possibilities = telephonePad.getKnightMove(i, j);
		else if (chessPiece.equals("bishop"))
			possibilities = telephonePad.getDiagonal(i, j);
		else if (chessPiece.equals("rook")) {
			possibilities = telephonePad.getHorizontal(i, j);
			possibilities = merge(possibilities,
					(telephonePad.getVertical(i, j)));
		} else if (chessPiece.equals("king")) {
			possibilities = telephonePad.getSurrounding(i, j);
		} else if (chessPiece.equals("pawn")) {
			possibilities = new ArrayList<Character>();
			// pawn only moves down the board
			if (telephonePad.isValidNumber(i + 1, j))
				possibilities.add(telephonePad.telePad[i + 1][j]);
		} else {
			throw new IllegalArgumentException();
		}

		return possibilities;
	}

	public static ArrayList<Character> merge(
			ArrayList<Character> possibilities, ArrayList<Character> newList) {
		ArrayList<Character> list = new ArrayList<Character>();
		list.addAll(possibilities);
		for (int ii = 0; ii < newList.size(); ii++)
			if (!list.contains(newList.get(ii)))
				list.add(newList.get(ii));
		return list;
	}

	public static class telephonePad {
		public static final int DEPTH = 4;
		public static final int LENGTH = 3;
		static char telePad[][] = { { '1', '2', '3' }, { '4', '5', '6', },
				{ '7', '8', '9' }, { '#', '0', '*' } };

		public static boolean isValidNumber(int i, int j) {

			return i < DEPTH && i >= 0 && j >= 0 && j < LENGTH
					&& Character.isDigit(telePad[i][j]);
		}

		public static int[] getIndexOf(char x) {
			int[] index = new int[2];

			for (int r = 0; r < DEPTH; r++) {
				for (int c = 0; c < LENGTH; c++) {
					if (telePad[r][c] == x) {
						index[0] = r;
						index[1] = c;
						return index;
					}
				}
			}
			index[0] = -1;
			index[1] = -1;
			return index;
		}

		// gets all values that are are on the row at telePad[i][j]
		public static ArrayList<Character> getHorizontal(int i, int j) {
			// check that you are at valid value
			if (!isValidNumber(i, j))
				throw new IllegalArgumentException();

			ArrayList<Character> val = new ArrayList<Character>();
			for (int col = 0; col < LENGTH; col++) {
				if (col != j)
					if (isValidNumber(i, col))
						val.add(telePad[i][col]);
			}
			return val;
		}

		public static ArrayList<Character> getVertical(int i, int j) {
			// check that you are at a valid value
			if (!isValidNumber(i, j))
				throw new IllegalArgumentException();

			ArrayList<Character> val = new ArrayList<Character>();

			for (int row = 0; row < DEPTH; row++) {
				if (row != i && isValidNumber(row, j))
					if (isValidNumber(row, j))
						val.add(telePad[row][j]);
			}
			return val;
		}

		public static ArrayList<Character> getDiagonal(int i, int j) {
			// check that you are at a valid value
			if (!isValidNumber(i, j))
				throw new IllegalArgumentException();

			ArrayList<Character> val = new ArrayList<Character>();

			int row = -1;
			int col = -1;

			while (true) {
				if (isValidNumber(i + row, j + col))
					val.add(telePad[i + row][j + col]);
				else
					break;
				row--;
				col--;
			}

			row = 1;
			col = 1;
			while (true) {
				if (isValidNumber(i + row, j + col))
					val.add(telePad[i + row][j + col]);
				else
					break;
				row++;
				col++;
			}

			row = 1;
			col = -1;
			while (true) {
				if (isValidNumber(i + row, j + col))
					val.add(telePad[i + row][j + col]);
				else
					break;
				row++;
				col--;
			}

			row = -1;
			col = 1;
			while (true) {
				if (isValidNumber(i + row, j + col))
					val.add(telePad[i + row][j + col]);
				else
					break;
				row--;
				col++;
			}

			return val;
		}

		public static ArrayList<Character> getSurrounding(int i, int j) {
			// returns all values surrounding [i,j]

			// check that you are at valid value
			if (!isValidNumber(i, j))
				throw new IllegalArgumentException();

			ArrayList<Character> val = new ArrayList<Character>();
			for (int r = -1; r < 2; r++)
				for (int c = -1; c < 2; c++) {
					if (isValidNumber(i + r, j + c) && !(r == 0 && c == 0))
						val.add(telePad[i + r][j + c]);
				}
			return val;
		}

		public static ArrayList<Character> getKnightMove(int i, int j) {
			// check that you are at valid value
			if (!isValidNumber(i, j))
				throw new IllegalArgumentException();

			ArrayList<Character> val = new ArrayList<Character>();

			// Two forward and one up, down
			if (isValidNumber(i + 1, j + 2))
				val.add(telePad[i + 1][j + 2]);
			if (isValidNumber(i - 1, j + 2))
				val.add(telePad[i - 1][j + 2]);
			// Two back and one up, down
			if (isValidNumber(i + 1, j - 2))
				val.add(telePad[i + 1][j - 2]);
			if (isValidNumber(i - 1, j - 2))
				val.add(telePad[i - 1][j - 2]);
			// Two up and one back, forward
			if (isValidNumber(i + 2, j + 1))
				val.add(telePad[i + 2][j + 1]);
			if (isValidNumber(i + 2, j - 1))
				val.add(telePad[i + 2][j - 1]);
			// Two down and one back, forward
			if (isValidNumber(i - 2, j + 1))
				val.add(telePad[i - 2][j + 1]);
			if (isValidNumber(i - 2, j - 1))
				val.add(telePad[i - 2][j - 1]);

			return val;
		}
	}

}
