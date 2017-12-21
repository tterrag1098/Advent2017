package advent_2017;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Day20Part2 {

	public static void main(String[] args) throws IOException {
		List<String> lines = Files.readAllLines(Paths.get("resources", "day20.txt"));
		List<int[][]> data = lines.stream()
				     .map(s -> s.replaceAll("[pva<>=]", ""))
				     .map(s -> s.replaceAll(",", " "))
				     .map(s -> s.split("\\s+"))
				     .map(arr -> Arrays.stream(arr).mapToInt(Integer::parseInt).toArray())
				     .map(a -> new int[][] { { a[0], a[1], a[2] },
				    		        	     { a[3], a[4], a[5] },
				    		        	     { a[6], a[7], a[8] } })
				     .collect(Collectors.toList());
		
		for (int i = 0; i < 10000; i++) {
			// simulate
			for (int[][] p : data) {
				for(int xyz = 0; xyz < 3; xyz++) {
					p[1][xyz] += p[2][xyz];
					p[0][xyz] += p[1][xyz];
				}
			}
			
			// remove collisions
			for (int j = 0; j < data.size(); j++) {
				int[][] p = data.get(j);
				boolean removed = false;
				for (int k = 0; k < data.size(); k++) {
					int[][] p2 = data.get(k);
					if (p != p2 && Arrays.equals(p[0], p2[0])) {
						if (!removed) {
							data.remove(j--);
							removed = true;
							k--;
						}
						data.remove(k--);
					}
				}
			}
		}
		
		System.out.println(data.size());
	}

}
