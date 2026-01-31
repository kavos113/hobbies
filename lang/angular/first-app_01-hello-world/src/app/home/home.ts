import { ChangeDetectorRef, Component, inject } from "@angular/core";
import { HousingLocation } from "../housing-location/housing-location";
import { HousingLocationInfo } from "../housinglocation";
import { Housing } from "../housing";

@Component({
  selector: "app-home",
  imports: [HousingLocation],
  template: `
    <section>
      <form>
        <input #filter type="text" placeholder="Filter by city" />
        <button
          type="submit"
          class="primary"
          (click)="filterResults(filter.value)"
        >
          Search
        </button>
      </form>
    </section>
    <section class="results">
      @for (house of filteredLocationList; track $index) {
        <app-housing-location [housingLocation]="house" />
      }
    </section>
  `,
  styleUrls: ["./home.css"],
})
export class Home {
  private readonly changeDetectorRef = inject(ChangeDetectorRef);
  housingLocationList: HousingLocationInfo[] = [];
  housingService: Housing = inject(Housing);
  filteredLocationList: HousingLocationInfo[] = [];

  constructor() {
    this.housingLocationList = this.housingService.getAllHousingLocations();
    this.filteredLocationList = this.housingLocationList;
  }

  filterResults(text: string) {
    if (!text) {
      this.filteredLocationList = this.housingLocationList;
      return;
    }

    console.log(`Filtering results with text: ${text}`);

    this.filteredLocationList = this.housingLocationList.filter((location) =>
      location.city.toLowerCase().includes(text.toLowerCase()),
    );
  }
}
