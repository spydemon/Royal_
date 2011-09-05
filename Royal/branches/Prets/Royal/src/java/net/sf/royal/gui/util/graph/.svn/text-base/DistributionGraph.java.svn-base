package net.sf.royal.gui.util.graph;

import java.util.List;

import net.sf.royal.gui.manager.LocaleManager;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PiePlot3D;
import org.jfree.data.general.DefaultPieDataset;

public class DistributionGraph {

    private DefaultPieDataset dataset = new DefaultPieDataset();;
    private ChartPanel chartPanel;
    
    
    public DistributionGraph(){
        chartPanel = new ChartPanel(this.createPie());
        chartPanel.setLocale(LocaleManager.getInstance().getCurrentLocale());
        
    }
    
    private JFreeChart createPie(){
        JFreeChart pie = ChartFactory.createPieChart3D("", dataset, false, false, false);
        PiePlot3D plot3 = (PiePlot3D) pie.getPlot();
        plot3.setForegroundAlpha(0.6f);
        plot3.setCircular(false);
        return pie;
    }

    public void setList(List<DistributionValue> list) {
        for(int i=0;i<list.size();i++){
            DistributionValue value = list.get(i);
            dataset.setValue(value.getName(), value.getValue());
        }
    }
    
    public void clear(){
        dataset.clear();
    }
    
    public ChartPanel getChartPanel() {
        return chartPanel;
    }

    public static DistributionValue getDistributionValue(String name, double value){
        return new DistributionValue(name, value);
    }
}
