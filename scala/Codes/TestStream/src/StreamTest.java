import java.util.Random;
import java.util.stream.IntStream;

public class StreamTest {
	public static void main(String[] args) {
		IntStream.range(1, 10).parallel().forEach(i -> {
			try {
				Thread.sleep(1000); // NETWORK, IO, CPU PROCESS
			} catch (InterruptedException e) {
			}
			System.out.println(i);
		});

		long start = System.nanoTime();
		doit();
		System.out.println((System.nanoTime() - start) / 1000000);
	}

	public static void doit() {
		Random r = new Random();
		double result = IntStream.range(1, 10000).parallel().map(x -> {
			return (int) IntStream.range(1, 100000).map(y -> r.nextInt(10000)).average().getAsDouble();
		}).average().getAsDouble();
		System.out.println(result);
	}
//	4999.073407340734
//	15,840
//	4998.852485248525
//	72,260

}
