package advent_2017;

public class Day23Part2 {

	public static void main(String[] args) {
		int a = 1, b, c, d, e, f, g, h = 0;
		b = 67;
		c = b;
		if (a != 0) {
			b *= 100;
			b += 100000;
			c = b;
			c += 17000;
		}
		do {
			f = 1;
			d = 2;
			if (!isPrime(b)) {
				h++;
			}
			g = b;
			g -= c;
			b += 17;
		} while (g != 0);
		System.out.println(h);
	}
	
	private static boolean isPrime(int x) {
		if (x <= 1) return false;
		if (x <= 3) return true;
		if (x % 2 == 0 || x % 3 == 0) return false;
		int i = 5;
		while (i * i <= x) {
			if (x % i == 0 || x % (i + 2) == 0) return false;
			i += 6;
		}
		return true;
	}

}
