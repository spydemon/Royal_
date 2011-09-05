package net.sf.royal.gui.util.graph;


public class GraphFactory {
    
    public static final String jan = "01";
    public static final String feb = "02";
    public static final String mar = "03";
    public static final String avr = "04";
    public static final String may = "05";
    public static final String jun = "06";
    public static final String jul = "07";
    public static final String aug = "08";
    public static final String sep = "09";
    public static final String oct = "10";
    public static final String nov = "11";
    public static final String dec = "12";

    public GraphFactory instance = new GraphFactory();
    
    private GraphFactory(){
        
    }
    
    public static DistributionGraph createDistributionGraph(){
        return new DistributionGraph();
    }
    
    public static LoanDetailGraph createLoanDetailGraph(){
        return new LoanDetailGraph();
    }
    
    public static LoanDaysGraph createLoanDaysGraph(){
        return new LoanDaysGraph();
    }
    
}
