package net.sf.royal.gui.manager;

import java.awt.Cursor;

import net.sf.royal.Royal;
import net.sf.royal.gui.util.WorkPanel;

public abstract class WorkManager implements Runnable{

    private Object[] args;
    private int maxValue;
    private int value = 0;
    
    public WorkManager(Object[] args, int maxValue){
        this.args = args;
        this.maxValue = maxValue;
    }

    public void run() {
        Royal.contentPane.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        WorkPanel.instance.setProgressBarMax(maxValue);
        WorkPanel.instance.setProgressBarValue(0, LocaleManager.getInstance().getString("initialization"), Boolean.TRUE.booleanValue());
        value++;
        this.doAction(args);
    }
    
    
    public void increment(String text){
        WorkPanel.instance.setProgressBarValue(value++, text, Boolean.TRUE.booleanValue());
    }
    
    public void endWork(){
        WorkPanel.instance.setProgressBarValue(maxValue, null, Boolean.FALSE.booleanValue());
        Royal.contentPane.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
    public void setError(){
        WorkPanel.instance.setError();
    }
    
    public void work(){
        new Thread(this).start();
    }
    
    public abstract void doAction(Object[] args);
    
    
}
