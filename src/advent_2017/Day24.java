package advent_2017;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public class Day24 {
	
	private static class Component {
		private final int a, b;
		private boolean usedA, usedB;
		
		Component(int a, int b) { this.a = a; this.b = b; }
		
		@Override
		public boolean equals(Object obj) {
			if (obj == null || obj.getClass() == getClass()) {
				return false;
			}
			Component other = (Component) obj;
			return other.a == this.a && other.b == this.b;
		}
		
		@Override
		public int hashCode() {
			return Integer.hashCode(a) ^ Integer.hashCode(b);
		}
	}

	public static void main(String[] args) throws IOException {
		Set<Component> components = 
				Files.readAllLines(Paths.get("resources", "day24.txt")).stream()
				.map(l -> Arrays.stream(l.split("/"))
								.mapToInt(Integer::parseInt)
								.toArray())
				.map(i -> new Component(i[0], i[1]))
				.collect(Collectors.toSet());
		
		
	}

}
