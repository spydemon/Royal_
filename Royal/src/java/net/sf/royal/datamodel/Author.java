package net.sf.royal.datamodel;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * 
 * @author Clément
 *
 * <p>This class represents a comic author (colourist, illustrator or scenarist)</p>
 * <p>Persisted class</p>
 */
@SuppressWarnings("serial")
public class Author extends ModelImpl {
    
    private String firstName;
    private String name;
    private String nickName;
    private String story;
    private String web;
    private Date birth;
    private Date death;
    private String photo;
    
    private Set<Work> works = new HashSet<Work>();
    
    /**
     * @return Returns the birth.
     */
    public Date getBirth() {
        return birth;
    }
    /**
     * @param birth The birth to set.
     */
    public void setBirth(Date birth) {
        this.birth = birth;
    }
    /**
     * @return Returns the death.
     */
    public Date getDeath() {
        return death;
    }
    /**
     * @param death The death to set.
     */
    public void setDeath(Date death) {
        this.death = death;
    }
    /**
     * @return Returns the firstName.
     */
    public String getFirstName() {
        return firstName;
    }
    /**
     * @param firstName The firstName to set.
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }
    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }
    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * @return Returns the nickName.
     */
    public String getNickName() {
        return nickName;
    }
    /**
     * @param nickName The nickName to set.
     */
    public void setNickName(String nickName) {
        this.nickName = nickName;
    }
    /**
     * @return Returns the story.
     */
    public String getStory() {
        return story;
    }
    /**
     * @param story The story to set.
     */
    public void setStory(String story) {
        this.story = story;
    }
    /**
     * @return Returns the web.
     */
    public String getWeb() {
        return web;
    }
    /**
     * @param web The web to set.
     */
    public void setWeb(String web) {
        this.web = web;
    }
    /**
     * @return Returns the works.
     */
    public Set<Work> getWorks() {
        return works;
    }
    /**
     * @param works The works to set.
     */
    public void setWorks(Set<Work> works) {
        this.works = works;
    }
    
    public void addWork(Work arg0) {
        works.add(arg0);
    }
    
    public void removeWork(Work arg0) {
        works.remove(arg0);
    }
    /**
     * @return Returns the photo.
     */
    public String getPhoto() {
        return photo;
    }
    /**
     * @param photo The photo to set.
     */
    public void setPhoto(String photo) {
        this.photo = photo;
    }

	@Override
	public String toString() {
		if(this.name == null || this.name.equals(""))
			return this.nickName;
		else
			return this.name + " " + this.firstName;
	}
    
}
