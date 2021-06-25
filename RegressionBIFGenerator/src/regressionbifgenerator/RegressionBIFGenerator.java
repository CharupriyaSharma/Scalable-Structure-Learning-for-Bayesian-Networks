/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package regressionbifgenerator;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author gross
 */
public class RegressionBIFGenerator {

    private final int[] degreeWeights;
    private final int numberOfParents;
    private final int numberOfInteractionTerms;
    private final File outputFile;
    private final Random rng;
    private final Set<BitSet> interactionTerms;
    private final Map<BitSet, Integer> probabilities;

    public RegressionBIFGenerator(int[] degreeWeights, int numberOfParents, int numberOfInteractionTerms, File outputFile) {
        this.degreeWeights = degreeWeights;
        this.numberOfParents = numberOfParents;
        this.numberOfInteractionTerms = (int) Math.min(numberOfInteractionTerms, Math.pow(2, numberOfParents));
        this.outputFile = outputFile;
        this.rng = new Random();
        this.interactionTerms = new HashSet<>();
        this.probabilities = new HashMap<>();
    }

    public void generate() {
        for (int i = 0; i < numberOfInteractionTerms; i++) {
            boolean failure;
            BitSet interactionTerm;
            do {
                interactionTerm = generateInteractionTerm();
                failure = !interactionTerms.add(interactionTerm);
            } while (failure);
            probabilities.put(interactionTerm, generateProbability());
        }
        writeBIFFile();
    }

    protected BitSet generateInteractionTerm() {
        BitSet result = new BitSet(numberOfParents);
        int maxSum = Arrays.stream(degreeWeights).sum();
        int random = rng.nextInt(maxSum);
        int sum = 0;
        int degree = 4;
        for (int i=0; i<degreeWeights.length; i++) {
            sum = sum + degreeWeights[i];
            if (random < sum) {
                degree = i+1;
                break;
            }            
        }
        for (int i = 0; i < degree; i++) {
            boolean failure = true;
            do {
                int candidate = rng.nextInt(numberOfParents);
                failure = result.get(candidate);
                result.set(candidate);
            } while (failure);
        }
        return result;
    }

    protected int generateProbability() {
        return (rng.nextInt(99) + 1);
    }

    protected void writeBIFFile() {
        try (FileWriter writer = new FileWriter(outputFile)) {
            writer.write("network unknown {\n");
            writer.write("}\n");
            for (int i = 0; i <= numberOfParents; i++) {
                writer.write("variable x" + i + " {\n");
                writer.write("  type discrete [ 2 ] { 0, 1 };\n");
                writer.write("}\n");
            }
            for (int i = 1; i <= numberOfParents; i++) {
                writer.write("probability ( x" + i + " ) {\n");
                writer.write("  table 0.5, 0.5;\n");
                writer.write("}\n");
            }            
            writer.write("probability ( x0 | ");
            for (int i = 1; i <= numberOfParents; i++) {
                writer.write("x" + i);
                if (i < numberOfParents) {
                    writer.write(", ");
                }
            }
            writer.write(" ) {\n");
            for (int i = 0; i < Math.pow(2, numberOfParents); i++) {
                BitSet bitset = BitSet.valueOf(new long[] {i} );                
                writer.write("  (");
                for (int j = 0; j < numberOfParents; j++) {
                    if (bitset.get(j)) {
                        writer.write("1");
                    } else {
                        writer.write("0");
                    }
                    if (j < numberOfParents-1) {
                        writer.write(", ");
                    }
                }
                if (probabilities.containsKey(bitset)) {
                    System.out.println(bitset);
                    writer.write(String.format(") 0.%1$d, 0.%2$d;\n", 100 - probabilities.get(bitset), probabilities.get(bitset)));
                } else {
                    writer.write(") " + (1) + ", " + 0 + ";\n");
                }
            }
            writer.write("}\n");
            //probability ( DrivHist | DrivingSkill, RiskAversion ) {
            //(SubStandard, Psychopath) 0.001, 0.004, 0.995;
            writer.flush();
        } catch (IOException ex) {
            Logger.getLogger(RegressionBIFGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void printInteractionTerms() {
        for (BitSet interactionTerm : interactionTerms) {
            System.out.println(interactionTerm);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (args.length < 7) {
            System.out.println("Error: Four parameters needed: # parents, # regression terms, maximum degree of interaction terms, output file name.");
        }
        System.out.println("# parents = " + args[0]);
        int numberOfParents = Integer.parseInt(args[0]);
        System.out.println("# interaction terms = " + args[1]);
        int numberOfInteractionTerms = Integer.parseInt(args[1]);
        System.out.println("Degree weights of interaction terms = " + args[2] + " " + args[3] + " " + args[4] + " " + args[5]);
        int[] degreeWeights = new int[4];
        for (int i=2; i<= 5; i++) {
            degreeWeights[i-2] = Integer.parseInt(args[i]);
        }
        File outputFile = new File(args[6]);
        System.out.println("Output file: " + outputFile.getAbsolutePath());
        System.out.print("Generating network...");
        RegressionBIFGenerator generator = new RegressionBIFGenerator(degreeWeights, numberOfParents, numberOfInteractionTerms, outputFile);
        generator.generate();
        //generator.printRegressionTerms();
        System.out.println(" done.");
    }
}
