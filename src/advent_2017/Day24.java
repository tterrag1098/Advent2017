package advent_2017;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Day24 {
	
	private static class Component {
		private final int a, b;
		private final boolean usedA, usedB;
		
		Component(int a, int b) {
			this(a, b, false, false);
		}
		
		private Component(int a, int b, boolean usedA, boolean usedB) {	 
			this.a = a; 
			this.b = b;
			this.usedA = usedA;
			this.usedB = usedB;
		}
		
		Component used(boolean a) {
			return new Component(this.a, b, a || usedA, !a || usedB);
		}
		
		Component used(int socket) {
			return socket == a && !usedA ? used(true) : socket == b ? used(false) : this;
		}
		
		boolean available(int socket) {
			return (socket == a && !usedA) || (socket == b && !usedB);
		}

		public int nextAvailable() {
			if (!usedA) {
				return a;
			} else if (!usedB) {
				return b;
			} else {
				throw new IllegalStateException("No socket available");
			}
		}
		
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
		
		@Override
		public String toString() {
			return (usedA ? "X" : "O") + "-" + a + "/" + (usedB ? "X" : "O") + "-" + b;
		}
	}

	public static void main(String[] args) throws IOException {
		List<Component> components = 
				Files.readAllLines(Paths.get("resources", "day24.txt")).stream()
				.map(l -> Arrays.stream(l.split("/"))
								.mapToInt(Integer::parseInt)
								.toArray())
				.map(i -> new Component(i[0], i[1]))
				.collect(Collectors.toList());

		
		Comparator<List<Component>> part1 = Comparator.comparingInt(Day24::strength);
		Comparator<List<Component>> part2 = Comparator.<List<Component>>comparingInt(List::size).thenComparingInt(Day24::strength);

		System.out.println("Part 1: " + strength(findBridge(components, part1)));
		System.out.println("Part 2: " + strength(findBridge(components, part2)));
	}
	
	private static int strength(List<Component> strongest) {
		return strongest.stream().mapToInt(c -> c.a + c.b).sum();
	}
	
	private static List<Component> findBridge(List<Component> components, Comparator<List<Component>> comparison) {
		List<Component> roots = components.stream().filter(c -> c.a == 0 || c.b == 0).collect(Collectors.toList());
		List<Component> strongest = new ArrayList<>();
		
		for (Component root : roots) {
			Set<Component> available = new HashSet<>();
			available.addAll(components);
			available.remove(root);
			
			List<Component> bridge = findBridge(root, available, comparison);
			if (comparison.compare(bridge, strongest) > 0) {
				strongest = bridge;
			}
		}
		
		return strongest;
	}

	private static List<Component> findBridge(Component root, Set<Component> available, Comparator<List<Component>> comparison) {
		List<Component> path = new ArrayList<>();
		path.add(root.used(0));
		return findBridge(path, new ArrayList<>(), available, comparison);
	}
	
	private static List<Component> findBridge(List<Component> path, List<Component> strongest, Set<Component> available, Comparator<List<Component>> comparison) {
		Component end = path.get(path.size() - 1);
		Deque<Component> possible = available.stream().filter(c -> c.available(end.nextAvailable())).collect(Collectors.toCollection(ArrayDeque::new));

		while (!possible.isEmpty()) {
			Component next = possible.pop();
			List<Component> newPath = new ArrayList<>(path);
			newPath.set(path.size() - 1, end.used(end.nextAvailable()));
			newPath.add(next.used(end.nextAvailable()));
			if (comparison.compare(newPath, strongest) > 0) {
				strongest = new ArrayList<>(newPath);
			}
			Set<Component> newAvailable = new HashSet<>(available);
			newAvailable.remove(next);
			strongest = findBridge(newPath, strongest, newAvailable, comparison);
		}
		return strongest;
	}
}
